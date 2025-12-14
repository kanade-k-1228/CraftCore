mod error;
mod label;
mod parser;

use error::Error;
use indexmap::IndexMap;
use parser::Stmt;
use std::{fs::File, io::BufReader};

#[derive(Debug, clap::Parser)]
#[clap(author, version, about)]
struct Args {
    /// Input files
    #[clap(default_value = "main.rk")]
    input: Vec<String>,

    /// Output file
    #[clap(short, long, default_value = "main.rk.bin")]
    output: String,

    /// Dump assembly code
    #[clap(short, long)]
    dump: bool,
}

fn main() -> Result<(), Error> {
    use clap::Parser;
    use std::io::{BufRead, Write};

    let args: Args = Args::parse();
    println!("RK16 Assembler by kanade-k-1228");

    println!("1. Read files");
    let files = {
        let mut files: IndexMap<String, Vec<String>> = IndexMap::new(); // (file, lines)
        for path in &args.input {
            println!("  < {}", path);
            let file = File::open(path).map_err(|e| Error::FileOpen(path.clone(), e))?;
            let mut lines = vec![];
            for line in BufReader::new(file).lines() {
                let raw = line.map_err(|e| Error::FileRead(e))?;
                lines.push(raw);
            }
            files.insert(path.clone(), lines);
        }
        files
    };

    println!("2. Parse lines");
    let parsed: IndexMap<String, Vec<(Option<Stmt>, Option<String>)>> = {
        let mut result = IndexMap::new();

        for (path, file_lines) in &files {
            let mut parsed_lines = vec![];

            for (idx, raw) in file_lines.iter().enumerate() {
                // Split comment
                let (code, comment) = match raw.split_once(';') {
                    Some((code, comment)) => (code.to_string(), Some(comment.to_string())),
                    None => (raw.to_string(), None),
                };

                // Parse content
                let (content, errors) = Stmt::parse(&code);

                // Print parse errors
                for error in errors {
                    error.print_diag(&files, path.as_str(), idx);
                }

                parsed_lines.push((content, comment));
            }

            result.insert(path.clone(), parsed_lines);
        }

        result
    };

    println!("3. Collect labels and calculate PC");
    let (parsed, labels) = {
        let mut pc: u16 = 0;
        let mut resolved = IndexMap::new();
        let mut labels = label::Labels::new();

        for (path, parsed_lines) in parsed {
            let mut updated_lines = vec![];

            for (content, comment) in parsed_lines {
                let updated_content = match content {
                    Some(parser::Stmt::CodeLabel(key)) => {
                        // Codeラベルの場合、現在のpcを記録
                        if let Some(_prev) = labels.insert(
                            key.clone(),
                            (path.clone(), updated_lines.len()),
                            label::LabelType::Code,
                        ) {
                            // 重複ラベルの警告は一旦省略
                        }
                        labels.set_pc(key.clone(), pc);
                        Some(parser::Stmt::CodeLabel(key))
                    }
                    Some(parser::Stmt::StaticLabel(key, val)) => {
                        if let Some(_prev) = labels.insert(
                            key.clone(),
                            (path.clone(), updated_lines.len()),
                            label::LabelType::Static(val),
                        ) {
                            // 重複ラベルの警告は一旦省略
                        }
                        Some(parser::Stmt::StaticLabel(key, val))
                    }
                    Some(parser::Stmt::ConstLabel(key, val)) => {
                        if let Some(_prev) = labels.insert(
                            key.clone(),
                            (path.clone(), updated_lines.len()),
                            label::LabelType::Const(val),
                        ) {
                            // 重複ラベルの警告は一旦省略
                        }
                        Some(parser::Stmt::ConstLabel(key, val))
                    }
                    Some(parser::Stmt::Asm(asm, _)) => {
                        let current_pc = pc;
                        pc += 1;
                        Some(parser::Stmt::Asm(asm, Some(current_pc)))
                    }
                    None => None,
                };

                updated_lines.push((updated_content, comment));
            }

            resolved.insert(path, updated_lines);
        }

        (resolved, labels)
    };

    // Pass 4: Resolve labels
    println!("4. Resolve labels");

    let mut resolved_instructions = vec![];
    for (path, parsed_lines) in &parsed {
        for (idx, (content, _comment)) in parsed_lines.iter().enumerate() {
            if let Some(parser::Stmt::Asm(asm, _pc)) = content {
                match asm.resolve(&labels) {
                    Ok(inst) => resolved_instructions.push(inst),
                    Err(err) => {
                        err.print_diag(&files, path.as_str(), idx);
                    }
                }
            }
        }
    }

    // Pass 5: Generate binary
    println!("5. Generate binary");
    println!("  > {}", &args.output);
    let mut file =
        File::create(&args.output).map_err(|e| Error::FileCreate(args.output.clone(), e))?;

    for inst in &resolved_instructions {
        let bin = inst.clone().to_op().to_bin();
        file.write(&bin.to_le_bytes())
            .map_err(|e| Error::FileWrite(args.output.clone(), e))?;
    }

    if args.dump {
        use color_print::cformat;
        for (path, parsed_lines) in &parsed {
            for (idx, (content, comment)) in parsed_lines.iter().enumerate() {
                // Print file header for first line
                let file_header = if idx == 0 {
                    format!(
                        "{}+------[{}]{}\n",
                        "-".repeat(19),
                        path,
                        "-".repeat(45 - path.len())
                    )
                } else {
                    String::new()
                };

                let comment_str = comment
                    .as_ref()
                    .map(|s| format!(";{}", s))
                    .unwrap_or_default();

                let line_num = idx + 1;
                let body = match content {
                    None => {
                        format!("{:19}| {:>4}: {}", "", line_num, comment_str)
                    }
                    Some(parser::Stmt::Asm(asm, pc_opt)) => {
                        let bin_str = match asm.resolve(&labels) {
                            Ok(inst) => {
                                let bin = inst.clone().to_op().to_bin();
                                format!(
                                    "{:02X} {:02X} {:02X} {:02X}",
                                    (bin >> 24) & 0xFF,
                                    (bin >> 16) & 0xFF,
                                    (bin >> 8) & 0xFF,
                                    bin & 0xFF
                                )
                            }
                            Err(_) => cformat!("<r,s>!! !! !! !!</>"),
                        };
                        // Use the PC value from the Option
                        let pc_str = pc_opt
                            .map(|pc| format!("{:04X}", pc))
                            .unwrap_or_else(|| "????".to_string());
                        format!(
                            "[{}] {} | {:>4}:   {} {}",
                            pc_str,
                            bin_str,
                            line_num,
                            asm.cformat(&labels),
                            comment_str
                        )
                    }
                    Some(parser::Stmt::CodeLabel(key)) => {
                        let label = cformat!("<g>{}:</>", key);
                        format!("{:19}| {:>4}: {} {}", "", line_num, label, comment_str)
                    }
                    Some(parser::Stmt::StaticLabel(key, val)) => {
                        let label = cformat!("<c>@0x{:04X} {}</>", val, key);
                        format!("{:19}| {:>4}: {} {}", "", line_num, label, comment_str)
                    }
                    Some(parser::Stmt::ConstLabel(key, val)) => {
                        let label = cformat!("<y>#0x{:04X} {}</>", val, key);
                        format!("{:19}| {:>4}: {} {}", "", line_num, label, comment_str)
                    }
                };

                print!("{}{}", file_header, body);
                println!();
            }
        }
        println!("-------------------+-----------------------------------------------------");
    }

    Ok(())
}
