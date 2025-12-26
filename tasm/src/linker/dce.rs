use crate::convert::Code;
use std::collections::{HashMap, HashSet};

/// Dead Code Elimination - removes unused functions and data items
pub struct DeadCodeEliminator {
    /// Set of entry points that must be kept
    entry_points: HashSet<String>,
    /// Set of reachable symbols
    reachable: HashSet<String>,
    /// Dependency graph: symbol -> set of symbols it references
    dependencies: HashMap<String, HashSet<String>>,
}

impl DeadCodeEliminator {
    /// Create a new DCE with default entry points
    pub fn new() -> Self {
        let mut entry_points = HashSet::new();
        // Standard entry points
        entry_points.insert("reset".to_string());
        entry_points.insert("irq".to_string());
        entry_points.insert("main".to_string());

        Self {
            entry_points,
            reachable: HashSet::new(),
            dependencies: HashMap::new(),
        }
    }

    /// Add a custom entry point
    pub fn add_entry_point(&mut self, name: &str) {
        self.entry_points.insert(name.to_string());
    }

    /// Build dependency graph from code
    pub fn analyze_code(&mut self, codes: &HashMap<&str, Code>) {
        // Build dependency graph by analyzing each function's code
        for (&name, code) in codes {
            let mut refs = HashSet::new();

            // Scan instructions for symbol references
            for (_, label_opt) in &code.0 {
                if let Some(label) = label_opt {
                    // This instruction references another symbol
                    refs.insert(label.clone());
                }
            }

            self.dependencies.insert(name.to_string(), refs);
        }
    }

    /// Perform reachability analysis starting from entry points
    pub fn find_reachable(&mut self) {
        let mut worklist: Vec<String> = self.entry_points.iter().cloned().collect();
        self.reachable = self.entry_points.clone();

        while let Some(symbol) = worklist.pop() {
            if let Some(deps) = self.dependencies.get(&symbol) {
                for dep in deps {
                    if self.reachable.insert(dep.clone()) {
                        // First time seeing this symbol, add to worklist
                        worklist.push(dep.clone());
                    }
                }
            }
        }
    }

    /// Check if a symbol is reachable (should be kept)
    pub fn is_reachable(&self, name: &str) -> bool {
        self.reachable.contains(name)
    }

    /// Filter items to keep only reachable ones
    pub fn filter_items<T>(&self, items: HashMap<String, T>) -> HashMap<String, T> {
        items
            .into_iter()
            .filter(|(name, _)| self.is_reachable(name))
            .collect()
    }

    /// Get statistics about eliminated code
    pub fn get_stats(&self, total_items: usize) -> (usize, usize) {
        let kept = self.reachable.len();
        let removed = total_items.saturating_sub(kept);
        (kept, removed)
    }
}
