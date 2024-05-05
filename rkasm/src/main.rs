mod parser;
use clap::Parser;
use color_print::{cformat, cprintln};
use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    let args: AppArgs = AppArgs::parse();
    println!("RK16 Assembler by kanade-k-1228");

    println!("----------------------------------------------------");
    println!("1. Parse Files: ");
    let lines: Vec<parser::Line> = args
        .input
        .iter()
        .flat_map(|fpath| {
            println!("  - {}", fpath);
            let file =
                File::open(fpath).expect(&cformat!("<red,bold>Cannot Open File</>: {}", fpath));
            let buf = BufReader::new(file);
            buf.lines()
                .enumerate()
                .map(|(idx, line)| parser::Line::parse(fpath, idx, &line.unwrap()))
                .collect::<Vec<parser::Line>>()
        })
        .collect();
    for line in &lines {
        println!("{}", line.cprint());
    }

    println!("2. Collecting Label ... ");
    let labels = {
        let mut labels = HashMap::<String, u16>::new();
        let mut pc_cnt: u16 = 0;
        for line in &lines {
            if let Some(ref stmt) = line.stmt {
                match stmt {
                    parser::Stmt::Op(_) => pc_cnt += 1,
                    parser::Stmt::CodeLabel { ref label, .. } => {
                        if let Some(_) = labels.insert(label.clone(), pc_cnt) {
                            println!("----------------------------------------------------");
                            cprintln!("<red>Error</>: Duplicate Label");
                            println!("{}: {}", line.print_pos(), label);
                            println!("----------------------------------------------------");
                        }
                    }
                    parser::Stmt::AddrLabel { ref label, value } => {
                        if let Some(_) = labels.insert(label.clone(), *value) {
                            println!("----------------------------------------------------");
                            cprintln!("<red>Error</>: Duplicate Label");
                            println!("{}: {}", line.print_pos(), label);
                            println!("----------------------------------------------------");
                        }
                    }
                    parser::Stmt::ConstLabel { ref label, value } => {
                        if let Some(_) = labels.insert(label.clone(), *value) {
                            println!("----------------------------------------------------");
                            cprintln!("<red>Error</>: Duplicate Label");
                            println!("{}: {}", line.print_pos(), label);
                            println!("----------------------------------------------------");
                        }
                    }
                    _ => {}
                };
            }
        }
        labels
    };
    println!("  - found #{} labels", labels.len());

    println!("3. Resolve Label");

    println!("4. Generate Binary");

    println!("5. Output Binary");

    println!("----------------------------------------------------");
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
}
