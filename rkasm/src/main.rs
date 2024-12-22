mod label;
#[allow(dead_code)]
mod msg;
mod parser;

use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use clap::Parser;
use color_print::cformat;

use label::Labels;
use msg::Msgs;
use parser::{Code, Line, Stmt};

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

    let mut msgs = Msgs::new();

    println!("1. Read Files and Parse Lines");
    let lines = {
        let mut lines = vec![];
        for fpath in &args.input {
            println!("  - {}", fpath);
            let file =
                File::open(fpath).expect(&cformat!("<red,bold>Failed to open File</>: {}", fpath));
            for (idx, line) in BufReader::new(file).lines().enumerate() {
                lines.push(Line::new(
                    &fpath,
                    idx,
                    &line.expect(&cformat!("Failed to read line")),
                ));
            }
        }
        lines
    };

    println!("2. Parse Codes");
    let codes = {
        let mut codes = vec![];
        let mut pc = 0;
        for line in lines {
            let (code, msg) = Code::parse(line, pc);
            if let Some(Stmt::Op { .. }) = &code.stmt {
                pc += 1;
            }
            msgs.extend(msg);
            codes.push(code);
        }
        codes
    };
    msgs.flush();

    println!("3. Collect Label");
    let labels = {
        let mut labels = Labels::new();
        for code in &codes {
            if let Some(Stmt::Label(lab)) = &code.stmt {
                if let Some((prev, _, _)) =
                    labels.insert(lab.key.clone(), (code.line.clone(), lab.clone(), lab.val))
                {
                    msgs.error(format!("Re-defined label `{}`", lab.key), code.line.clone());
                    msgs.note(format!("Already defined here"), prev.clone());
                }
            }
        }
        labels
    };
    msgs.flush();

    println!("4. Resolve Label & Generate Binary");
    for code in &codes {
        msgs.extend(code.resolve(&labels));
        msgs.extend(code.generate_bin());
    }
    msgs.flush();

    if args.dump {
        for line in &codes {
            println!("{}", line.cformat());
        }
        println!("+-----+------+-------------+-----------------------------+");
    }
}
