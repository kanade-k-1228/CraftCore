use std::collections::HashSet;

use super::code::Imm;
use super::global::Global;
use crate::error::Error;

impl<'a> Global<'a> {
    pub fn deps(
        &'a self,
        entries: &[&str],
        mut labels: HashSet<String>,
        mut symbols: HashSet<String>,
    ) -> Result<(HashSet<String>, HashSet<String>), Error> {
        for entry in entries {
            (labels, symbols) = self.deps_rec(entry, labels, symbols)?;
        }
        Ok((labels, symbols))
    }

    fn deps_rec(
        &'a self,
        entry: &str,
        mut labels: HashSet<String>,
        mut symbols: HashSet<String>,
    ) -> Result<(HashSet<String>, HashSet<String>), Error> {
        if labels.contains(entry) {
            return Ok((labels, symbols));
        }

        // Silently skip undefined entry points (e.g. an unused "irq" vector);
        // any other code-gen error should still propagate.
        let code = match self.code(entry) {
            Ok(code) => code,
            Err(Error::UnknownIdentifier(_, _)) => return Ok((labels, symbols)),
            Err(e) => return Err(e),
        };
        labels.insert(entry.to_string());

        for inst in &code.0 {
            match inst.imm() {
                Some(Imm::Label(s)) => {
                    (labels, symbols) = self.deps_rec(s, labels, symbols)?;
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
