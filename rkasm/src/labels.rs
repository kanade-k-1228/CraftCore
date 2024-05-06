use std::collections::HashMap;

use color_print::cprintln;

use crate::parser::{Label, Line, Stmt};

pub fn collect_label(lines: &Vec<Line>) -> HashMap<String, u16> {
    let mut labels = HashMap::<String, u16>::new();
    let mut pc_cnt: u16 = 0;
    for line in lines {
        if let Some(ref stmt) = line.stmt {
            match stmt {
                Stmt::Op(_) => pc_cnt += 1,
                Stmt::Label(label) => match label {
                    Label::Code { label } => {
                        if let Some(_) = labels.insert(label.clone(), pc_cnt) {
                            println!("----------------------------------------------------");
                            cprintln!("<red>Error</>: Duplicate Label");
                            println!("{}: {}", line.print_pos(), label);
                            println!("----------------------------------------------------");
                        }
                    }
                    Label::Addr { label, value } => {
                        if let Some(_) = labels.insert(label.clone(), *value) {
                            println!("----------------------------------------------------");
                            cprintln!("<red>Error</>: Duplicate Label");
                            println!("{}: {}", line.print_pos(), label);
                            println!("----------------------------------------------------");
                        }
                    }
                    Label::Const { label, value } => {
                        if let Some(_) = labels.insert(label.clone(), *value) {
                            println!("----------------------------------------------------");
                            cprintln!("<red>Error</>: Duplicate Label");
                            println!("{}: {}", line.print_pos(), label);
                            println!("----------------------------------------------------");
                        }
                    }
                },
                _ => {}
            };
        }
    }
    labels
}
