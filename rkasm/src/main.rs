mod label;
#[allow(dead_code)]
mod msg;
mod parser;

use std::{
    fs::File,
    io::{BufRead, BufReader, Write},
};

use clap::Parser;
use color_print::cformat;

use label::Labels;
use msg::Msg;
use parser::Line;

#[derive(Parser, Debug)]
#[clap(
    name = "RK16 Assembler",
    author = "kanade-k-1228",
    version = "v1.0.0",
    about = "Assembler for RK16 ISA"
)]
struct Args {
    #[clap(default_value = "main.rk")]
    input: Vec<String>,
    #[clap(short, long, default_value = "out.rk.bin")]
    output: String,
    #[clap(short, long)]
    dump: bool,
}

fn main() {
    let args: Args = Args::parse();
    println!("RK16 Assembler by kanade-k-1228");

    println!("1. Read Files and Parse Lines");

    let mut lines = vec![];
    let mut pc: u16 = 0;
    let mut labels = Labels::new();

    for path in &args.input {
        println!("  - {}", path);
        let file = File::open(path).expect(&cformat!("<red,bold>Failed to open File</>: {}", path));
        for (idx, raw) in BufReader::new(file).lines().enumerate() {
            let raw = raw.expect(&cformat!("Failed to read line"));
            let (line, msgs) = Line::parse(path, idx, &raw, pc);
            if let Some(_) = line.get_op() {
                pc += 1;
            }

            // Collect Label
            if let Some(lab) = &line.get_label() {
                if let Some(prev) = labels.insert(lab.get_key(), line.clone()) {
                    Msg::Error(format!("Re-defined label: `{}`", lab.get_key()))
                        .print(line.get_info());
                    Msg::Note(format!("Already defined here")).print(prev.get_info());
                }
            }

            // Print Messages
            for msg in msgs {
                msg.print(line.get_info());
            }
            lines.push(line);
        }
    }

    println!("2. Resolve Label & Generate Binary");
    println!("  - out: {}", args.output);
    let mut file = File::create(&args.output).expect(&cformat!(
        "<red,bold>Failed to create File</>: {}",
        args.output
    ));
    for line in &lines {
        if let Some(asm) = &line.get_op() {
            let bin = asm.resolve(&labels).to_op().to_bin();
            file.write(&bin.to_le_bytes()).expect(&cformat!(
                "<red,bold>Failed to write File</>: {}",
                args.output
            ));
        }
    }

    if args.dump {
        for line in &lines {
            println!("{}", line.cformat(&labels));
        }
        println!("+------+------+-------------+-----------------------------+");
    }
}
