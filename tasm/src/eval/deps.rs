use std::collections::HashSet;

use super::code::Imm;
use super::global::Global;
use crate::error::Error;

impl<'a> Global<'a> {
    pub fn deps(
        &'a self,
        entry: &str,
        mut labels: HashSet<String>,
        mut symbols: HashSet<String>,
    ) -> Result<(HashSet<String>, HashSet<String>), Error> {
        if labels.contains(entry) {
            return Ok((labels, symbols));
        }

        let code = match self.code(entry) {
            Ok(code) => code,
            Err(_) => return Ok((labels, symbols)),
        };
        labels.insert(entry.to_string());

        for inst in &code.0 {
            match inst.imm() {
                Some(Imm::Label(s)) => {
                    (labels, symbols) = self.deps(s, labels, symbols)?;
                }
                Some(Imm::Symbol(s, _)) => {
                    symbols.insert(s.clone());
                }
                _ => {}
            }
        }

        Ok((labels, symbols))
    }
}
