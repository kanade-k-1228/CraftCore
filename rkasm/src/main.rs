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
use parser::{Line, Stmt};

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
    let mut lines = vec![];
    for path in &args.input {
        println!("  - {}", path);
        let file = File::open(path).expect(&cformat!("<red,bold>Failed to open File</>: {}", path));
        for (idx, line) in BufReader::new(file).lines().enumerate() {
            lines.push(Line::new(
                &path,
                idx,
                &line.expect(&cformat!("Failed to read line")),
            ));
        }
    }

    println!("2. Parse Codes");
    let mut pc = 0;
    for line in &lines {
        let msg = line.parse(pc);
        if let Some(Stmt::Op { .. }) = line.stmt.get().unwrap() {
            pc += 1;
        }
        msgs.extend(msg);
    }
    msgs.flush();

    println!("3. Collect Label");
    let mut labels = Labels::new();
    for line in &lines {
        if let Some(Stmt::Label(lab)) = &line.stmt.get().unwrap() {
            if let Some((prev, _, _)) =
                labels.insert(lab.key.clone(), (line.clone(), lab.clone(), lab.val))
            {
                msgs.error(format!("Re-defined label `{}`", lab.key), line.clone());
                msgs.note(format!("Already defined here"), prev.clone());
            }
        }
    }
    msgs.flush();

    println!("4. Resolve Label & Generate Binary");
    for line in &lines {
        msgs.extend(line.resolve(&labels));
        msgs.extend(line.generate_bin());
    }
    msgs.flush();

    if args.dump {
        for line in &lines {
            println!("{}", line.cformat());
        }
        println!("+------+------+-------------+-----------------------------+");
    }
}
