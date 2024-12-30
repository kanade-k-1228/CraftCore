mod label;
mod msg;
mod parser;

use color_print::cformat;

#[derive(Debug, clap::Parser)]
#[clap(
    name = "RK16 Assembler",
    author = "kanade-k-1228",
    version = "v1.0.0",
    about = "Assembler for RK16 ISA"
)]
struct Args {
    #[clap(default_value = "main.rk")]
    input: Vec<String>,
    #[clap(short, long)]
    output: Option<String>,
    #[clap(short, long)]
    dump: bool,
}

fn main() {
    use clap::Parser;
    use std::io::{BufRead, Write};

    let args: Args = Args::parse();
    println!("RK16 Assembler by kanade-k-1228");

    println!("1. Read Files and Parse Lines");

    let mut lines = vec![];
    let mut pc: u16 = 0;
    let mut labels = label::Labels::new();

    for path in &args.input {
        println!("  - {}", path);
        let file = std::fs::File::open(path)
            .expect(&cformat!("<red,bold>Failed to open File</>: {}", path));
        for (idx, raw) in std::io::BufReader::new(file).lines().enumerate() {
            let raw = raw.expect(&cformat!("Failed to read line"));
            let (line, msgs) = parser::Line::parse(path, idx, &raw, pc);
            if let Some(_) = line.get_op() {
                pc += 1;
            }

            // Collect Label
            if let Some(lab) = &line.get_label() {
                if let Some(prev) = labels.insert(lab.get_key(), line.clone()) {
                    msg::Msg::Warn(format!("Re-defined label: `{}`", lab.get_key()))
                        .diag(line.get_info());
                    msg::Msg::Note(format!("Already defined here. The value has been overridden. If this is not intentional, please reorder the sourcefile."))
                        .diag(prev.get_info());
                }
            }

            // Print Messages
            for msg in msgs {
                msg.diag(line.get_info());
            }
            lines.push(line);
        }
    }

    println!("2. Resolve Label & Generate Binary");
    let out = args
        .output
        .unwrap_or(format!("{}.bin", args.input.get(0).unwrap().clone()));
    println!("  - out: {}", out);
    let mut file = std::fs::File::create(&out)
        .expect(&cformat!("<red,bold>Failed to create File</>: {}", &out));
    for line in &lines {
        if let Some(asm) = &line.get_op() {
            let bin = match asm.resolve(&labels) {
                Some(ok) => ok.to_op().clone().to_bin(),
                None => {
                    msg::Msg::Error(format!("Undefined label")).diag(line.get_info());
                    continue;
                }
            };
            file.write(&bin.to_le_bytes())
                .expect(&cformat!("<red,bold>Failed to write File</>: {}", &out));
        }
    }

    if args.dump {
        for line in &lines {
            println!("{}", line.cformat(&labels));
        }
        println!("+------+------+-------------+-----------------------------+");
    }
}
