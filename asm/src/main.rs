use rkasm::error::Error;
use rkasm::ident::{Ident, Idents};
use rkasm::parser::Stmt;
use rkasm::util;

use indexmap::IndexMap;
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

        for (file, lines) in &files {
            let mut parsed = vec![];

            for (idx, raw) in lines.iter().enumerate() {
                // Split comment
                let (code, comment) = match raw.split_once(';') {
                    Some((code, comment)) => (code.to_string(), Some(comment.to_string())),
                    None => (raw.to_string(), None),
                };

                // Parse content
                let (stmt, errors) = Stmt::parse(&code);

                for e in errors {
                    e.cprint(&files, file.as_str(), idx);
                }

                parsed.push((stmt, comment));
            }

            result.insert(file.clone(), parsed);
        }

        result
    };

    println!("3. Collect idents and calculate PC");
    let (parsed, idents) = {
        let mut pc: u16 = 0;
        let mut resolved = IndexMap::new();
        let mut idents = Idents::new();

        for (path, parsed_lines) in parsed {
            let mut updated_lines = vec![];
            for (idx, (content, comment)) in parsed_lines.into_iter().enumerate() {
                let pcfilled = match content {
                    Some(Stmt::Label(key)) => {
                        if let Some(_) =
                            idents.insert(key.clone(), (path.clone(), idx), Ident::Label, pc)
                        {
                            Error::RedefinedLabel(key.clone()).cprint(&files, &path, idx);
                        }
                        Some(Stmt::Label(key))
                    }
                    Some(Stmt::Static(key, val)) => {
                        if let Some(_) =
                            idents.insert(key.clone(), (path.clone(), idx), Ident::Static, val)
                        {
                            Error::RedefinedLabel(key.clone()).cprint(&files, &path, idx);
                        }
                        Some(Stmt::Static(key, val))
                    }
                    Some(Stmt::Const(key, val)) => {
                        if let Some(_) =
                            idents.insert(key.clone(), (path.clone(), idx), Ident::Const, val)
                        {
                            Error::RedefinedLabel(key.clone()).cprint(&files, &path, idx);
                        }
                        Some(Stmt::Const(key, val))
                    }
                    Some(Stmt::Code(asm, _)) => {
                        pc += 1;
                        Some(Stmt::Code(asm, Some(pc - 1)))
                    }
                    None => None,
                };

                updated_lines.push((pcfilled, comment));
            }

            resolved.insert(path, updated_lines);
        }

        (resolved, idents)
    };

    println!("4. Resolve labels");
    let resolved: IndexMap<String, Vec<(Option<Stmt>, Option<String>)>> = {
        let mut resolved = IndexMap::new();
        for (path, parsed) in &parsed {
            let mut resolved_lines = vec![];

            for (idx, (stmt, comment)) in parsed.iter().enumerate() {
                if let Some(Stmt::Code(code, pc)) = stmt {
                    match code.resolve(&idents) {
                        Ok(_) => {
                            resolved_lines
                                .push((Some(Stmt::Code(code.clone(), *pc)), comment.clone()));
                        }
                        Err(err) => {
                            err.cprint(&files, path.as_str(), idx);
                            resolved_lines
                                .push((Some(Stmt::Code(code.clone(), *pc)), comment.clone()));
                        }
                    }
                } else {
                    resolved_lines.push((stmt.clone(), comment.clone()));
                }
            }

            resolved.insert(path.clone(), resolved_lines);
        }

        resolved
    };

    // Pass 5: Generate binary
    println!("5. Generate binary");
    println!("  > {}", &args.output);
    let mut file =
        File::create(&args.output).map_err(|e| Error::FileCreate(args.output.clone(), e))?;

    // Extract and write instructions from the resolved structure
    for (_, resolved_lines) in &resolved {
        for (_, (content, _)) in resolved_lines.iter().enumerate() {
            if let Some(Stmt::Code(asm, _)) = content {
                if let Ok(inst) = asm.resolve(&idents) {
                    let bin = inst.to_op().to_bin();
                    file.write(&bin.to_le_bytes())
                        .map_err(|e| Error::FileWrite(args.output.clone(), e))?;
                }
            }
        }
    }

    if args.dump {
        util::print_dump(&resolved, &idents);
    }

    Ok(())
}
