use crate::eval::global::Global;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct SymbolMap {
    pub code: IndexMap<String, CodeEntry>,
    pub data: IndexMap<String, DataEntry>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CodeEntry {
    pub addr: usize,
    pub size: usize,
    pub stacks: IndexMap<String, usize>, // ローカル変数のスタックフレーム中の相対位置
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DataEntry {
    pub addr: usize,
    pub size: usize,
}

impl SymbolMap {
    pub fn generate<'a>(
        evaluator: &'a Global<'a>,
        imap: &IndexMap<String, usize>,
        dmap: &IndexMap<String, usize>,
    ) -> Self {
        // Generate code map (functions, seqs, and asm blocks)
        let mut code_map: IndexMap<String, CodeEntry> = IndexMap::new();

        // Add all code entries from imap
        for (name, addr) in imap.iter() {
            // Actual code length comes from the cached compiled code.
            let size = evaluator
                .code(name.as_str())
                .map(|c| c.0.len())
                .unwrap_or(0);

            // For functions, surface each local's FP-relative offset.
            let stacks: IndexMap<String, usize> = evaluator
                .get_func_locals(name.as_str())
                .map(|locals| {
                    locals
                        .into_iter()
                        .map(|(n, off)| (n, (off as i16) as u16 as usize))
                        .collect()
                })
                .unwrap_or_default();

            code_map.insert(
                name.clone(),
                CodeEntry {
                    addr: *addr,
                    size,
                    stacks,
                },
            );
        }

        // Generate data map (statics and constants)
        let mut data_map: IndexMap<String, DataEntry> = IndexMap::new();

        // Add static variables
        for name in evaluator.statics() {
            if let Some((norm_type, _)) = evaluator.get_static_resolved(name) {
                if let Some(&addr) = dmap.get(name) {
                    data_map.insert(
                        name.to_string(),
                        DataEntry {
                            addr,
                            size: norm_type.sizeof(),
                        },
                    );
                }
            }
        }

        // Add constants
        for name in evaluator.consts() {
            if let Some((norm_type, _, _)) = evaluator.get_const_resolved(name) {
                if let Some(&addr) = dmap.get(name) {
                    data_map.insert(
                        name.to_string(),
                        DataEntry {
                            addr,
                            size: norm_type.sizeof(),
                        },
                    );
                }
            }
        }

        SymbolMap {
            code: code_map,
            data: data_map,
        }
    }

    /// Serialize to YAML string with hexadecimal numbers
    pub fn to_yaml(&self) -> String {
        let mut yaml = String::new();

        // Write code section
        yaml.push_str("code:\n");
        for (name, entry) in &self.code {
            yaml.push_str(&format!("  {}:\n", name));
            yaml.push_str(&format!("    addr: 0x{:x}\n", entry.addr));
            yaml.push_str(&format!("    size: 0x{:x}\n", entry.size));
            yaml.push_str("    stacks:");
            if entry.stacks.is_empty() {
                yaml.push_str(" {}\n");
            } else {
                yaml.push('\n');
                for (stack_name, offset) in &entry.stacks {
                    yaml.push_str(&format!("      {}: 0x{:x}\n", stack_name, offset));
                }
            }
        }

        // Write data section
        yaml.push_str("data:\n");
        if self.data.is_empty() {
            yaml.push_str("  {}\n");
        } else {
            for (name, entry) in &self.data {
                yaml.push_str(&format!("  {}:\n", name));
                yaml.push_str(&format!("    addr: 0x{:x}\n", entry.addr));
                yaml.push_str(&format!("    size: 0x{:x}\n", entry.size));
            }
        }

        yaml
    }
}
