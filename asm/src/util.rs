use crate::ident::Idents;
use crate::parser::Stmt;
use color_print::cformat;
use indexmap::IndexMap;

pub fn print_dump(parsed: &IndexMap<String, Vec<(Option<Stmt>, Option<String>)>>, idents: &Idents) {
    for (path, parsed_lines) in parsed {
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
                Some(Stmt::Code(asm, pc_opt)) => {
                    let bin_str = match asm.resolve(&idents) {
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
                        asm.cformat(),
                        comment_str
                    )
                }
                Some(Stmt::Label(key)) => {
                    let label = cformat!("<g>{}:</>", key);
                    format!("{:19}| {:>4}: {} {}", "", line_num, label, comment_str)
                }
                Some(Stmt::Static(key, val)) => {
                    let label = cformat!("<c>@0x{:04X} {}</>", val, key);
                    format!("{:19}| {:>4}: {} {}", "", line_num, label, comment_str)
                }
                Some(Stmt::Const(key, val)) => {
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
