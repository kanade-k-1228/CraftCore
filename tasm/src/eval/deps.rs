use indexmap::IndexSet;

use super::code::Imm;
use super::global::Global;
use crate::error::Error;

impl<'a> Global<'a> {
    // IndexSet keeps discovery order so allocation results are deterministic
    pub fn deps(
        &'a self,
        entries: &[&str],
        mut labels: IndexSet<String>,
        mut symbols: IndexSet<String>,
    ) -> Result<(IndexSet<String>, IndexSet<String>), Error> {
        for entry in entries {
            (labels, symbols) = self.deps_rec(entry, labels, symbols)?;
        }
        Ok((labels, symbols))
    }

    fn deps_rec(
        &'a self,
        entry: &str,
        mut labels: IndexSet<String>,
        mut symbols: IndexSet<String>,
    ) -> Result<(IndexSet<String>, IndexSet<String>), Error> {
        if labels.contains(entry) {
            return Ok((labels, symbols));
        }

        // Undefined entry points (e.g. an unused "irq" vector) are already
        // filtered out by resolve_entry, so any error here is a real one.
        let code = self.code(entry)?;
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
