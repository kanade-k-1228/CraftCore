use crate::link::structs::AsmLine;

/// Unified code representation for both functions and assembly blocks
#[derive(Debug, Clone)]
pub struct Code {
    /// Name of the code block (function or asm block name)
    pub name: String,
    /// Instruction sequence with unresolved symbols
    pub instructions: Vec<AsmLine>,
    /// Whether this is a function (true) or asm block (false)
    pub is_function: bool,
}

impl Code {
    pub fn new_function(name: String, instructions: Vec<AsmLine>) -> Self {
        Code {
            name,
            instructions,
            is_function: true,
        }
    }

    pub fn new_asm_block(name: String, instructions: Vec<AsmLine>) -> Self {
        Code {
            name,
            instructions,
            is_function: false,
        }
    }
}

/// Extension trait for BiMap to add print functionality
pub trait BiMapExt<L, R> {
    fn print(&self);
}

impl<
        L: std::fmt::Display + std::hash::Hash + Eq + Clone,
        R: std::fmt::Display + std::hash::Hash + Eq + Clone + Ord,
    > BiMapExt<L, R> for bimap::BiMap<L, R>
{
    fn print(&self) {
        println!("=== Memory Map ===");
        // Convert to vec for sorting
        let mut entries: Vec<_> = Vec::new();
        for k in self.left_values() {
            if let Some(r) = self.get_by_left(k) {
                entries.push((k.clone(), r.clone()));
            }
        }
        entries.sort_by_key(|(_, v)| v.clone());

        for (k, v) in entries {
            println!("  {} -> {}", k, v);
        }
        println!();
    }
}
