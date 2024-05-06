mod message;
mod parser;
use clap::Parser;
use color_print::cformat;
use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
};

use crate::{
    message::{dump, Msg},
    parser::{Code, Label, Line, Stmt},
};

fn main() {
    let args: AppArgs = AppArgs::parse();
    println!("RK16 Assembler by kanade-k-1228");

    let mut msgs: Vec<Msg> = vec![];

    println!("1. Read Files ");
    let lines = args
        .input
        .iter()
        .flat_map(|fpath| {
            println!("  - {}", fpath);
            let file =
                File::open(fpath).expect(&cformat!("<red,bold>Failed to open File</>: {}", fpath));
            let lines = BufReader::new(file)
                .lines()
                .enumerate()
                .map(|(idx, line)| Line::new(fpath, idx, line.unwrap()))
                .collect::<Vec<_>>();
            lines
        })
        .collect::<Vec<_>>();

    println!("2. Parse Codes");
    let codes = lines
        .iter()
        .scan(0 as u16, |pc, line| {
            let (code, msg) = Code::parse(line, *pc);
            if let Some(Stmt::Op { .. }) = &code.stmt {
                *pc += 1;
            }
            msgs.extend(msg);
            Some(code)
        })
        .collect::<Vec<_>>();

    dump(&msgs);
    msgs.clear();

    println!("3. Collect Label");
    let labels = {
        let mut labels = HashMap::<String, (&Line, &Label, u16)>::new();
        codes.iter().for_each(|code| {
            if let Some(Stmt::Label(ref lab)) = code.stmt {
                if let Some((prev, _, _)) =
                    labels.insert(lab.key.clone(), (code.line, lab, lab.val))
                {
                    msgs.extend(vec![
                        Msg::error(format!("Re-defined label `{}`", lab.key), &code.line),
                        Msg::note(format!("Already defined here"), &prev),
                    ])
                }
            }
        });
        labels
    };

    dump(&msgs);
    msgs.clear();

    println!("4. Resolve Label");
    codes.iter().for_each(|code| {
        msgs.extend(code.resolve(&labels));
    });

    dump(&msgs);
    msgs.clear();

    println!("5. Generate Binary");
    codes.iter().for_each(|code| {
        code.generate_bin();
    });

    dump(&msgs);
    msgs.clear();

    if args.dump {
        for line in &codes {
            println!("{}", line.cformat());
        }
        println!("+-----+------+-------------+-----------------------------+");
    }
}

#[derive(Parser, Debug)]
#[clap(
    name = "RK16 Assembler",
    author = "kanade-k-1228",
    version = "v1.0.0",
    about = "Assembler for RK16 ISA"
)]
struct AppArgs {
    #[clap()]
    input: Vec<String>,
    #[clap(short = 'o', long = "output", default_value = "out.rk.bin")]
    output: String,
    #[clap(short = 'd', long = "dump")]
    dump: bool,
}
