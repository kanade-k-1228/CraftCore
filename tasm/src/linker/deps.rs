use crate::error::Error;
use crate::eval::code::Imm;
use crate::eval::global::Global;
use color_print::cprintln;
use std::collections::HashSet;

pub struct Deps {
    labels: HashSet<String>,
    symbols: HashSet<String>,
}

impl Deps {
    pub fn resolve<'a>(global: &'a Global<'a>, entries: &[&str]) -> Result<Self, Error> {
        let mut labels = HashSet::new();
        let mut symbols = HashSet::new();
        let mut visited = HashSet::new();
        let mut worklist: Vec<String> = entries.iter().map(|s| s.to_string()).collect();

        while let Some(current) = worklist.pop() {
            if !visited.insert(current.clone()) {
                continue;
            }

            let code = match global.code(&current) {
                Ok(code) => code,
                Err(_) => continue, // Skip missing entry points
            };
            labels.insert(current);

            for inst in &code.0 {
                match inst.imm() {
                    Some(Imm::Label(s)) => {
                        if !visited.contains(s) {
                            worklist.push(s.clone());
                        }
                    }
                    Some(Imm::Symbol(s, _)) => {
                        symbols.insert(s.clone());
                    }
                    _ => {}
                }
            }
        }

        Ok(Self { labels, symbols })
    }

    pub fn labels(&self) -> &HashSet<String> {
        &self.labels
    }

    pub fn symbols(&self) -> &HashSet<String> {
        &self.symbols
    }

    pub fn print(&self) {
        println!("------------------------------------------------------------");
        let mut labels: Vec<_> = self.labels.iter().map(|s| s.as_str()).collect();
        let mut symbols: Vec<_> = self.symbols.iter().map(|s| s.as_str()).collect();
        labels.sort();
        symbols.sort();
        cprintln!("<green>labels:</green> [{}]", labels.join(", "));
        cprintln!("<yellow>symbols:</yellow> [{}]", symbols.join(", "));
    }
}
