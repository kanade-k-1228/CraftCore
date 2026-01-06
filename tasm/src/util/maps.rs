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
    pub fn generate(
        evaluator: &Global,
        imap: &IndexMap<String, usize>,
        dmap: &IndexMap<String, usize>,
    ) -> Self {
        // Generate code map (functions, seqs, and asm blocks)
        let mut code_map: IndexMap<String, CodeEntry> = IndexMap::new();

        // Add all code entries from imap
        for (name, addr) in imap.iter() {
            // Get size from the code if available
            let size = if let Some(_) = evaluator.asms().get(name.as_str()) {
                // For asm blocks, we'd need to get the actual code size
                // For now, use a placeholder
                0
            } else if let Some(_) = evaluator.funcs().get(name.as_str()) {
                // For functions and seq blocks, we'd need to get the actual code size
                0
            } else {
                0
            };

            code_map.insert(
                name.clone(),
                CodeEntry {
                    addr: *addr,
                    size,
                    stacks: IndexMap::new(), // TODO: Local variable stack positions would go here
                },
            );
        }

        // Generate data map (statics and constants)
        let mut data_map: IndexMap<String, DataEntry> = IndexMap::new();

        // Add static variables
        for (name, (norm_type, _, _)) in evaluator.statics() {
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

        // Add constants
        for (name, (norm_type, _, _, _)) in evaluator.consts() {
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
