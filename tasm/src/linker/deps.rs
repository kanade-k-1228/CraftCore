use crate::eval::code::{Code, Imm};
use color_print::cprintln;
use indexmap::IndexMap;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

pub struct Deps<'a> {
    graph: IndexMap<&'a str, (HashSet<&'a str>, HashSet<&'a str>)>,
    _cache: RefCell<HashMap<&'a str, (HashSet<&'a str>, HashSet<&'a str>)>>,
}

impl<'a> Deps<'a> {
    pub fn build(codes: &'a IndexMap<&'a str, Code>) -> Self {
        let mut graph = IndexMap::new();
        for (&name, Code(code)) in codes {
            let mut labels = HashSet::new();
            let mut symbols = HashSet::new();
            for inst in code {
                match inst.imm() {
                    Some(Imm::Label(s)) => {
                        labels.insert(s.as_str());
                    }
                    Some(Imm::Symbol(s, _)) => {
                        symbols.insert(s.as_str());
                    }
                    _ => {}
                }
            }
            graph.insert(name, (labels, symbols));
        }
        Self {
            graph,
            _cache: RefCell::new(HashMap::new()),
        }
    }

    pub fn entries(&self, entries: &[&'a str]) -> (HashSet<&'a str>, HashSet<&'a str>) {
        let mut all_labels = HashSet::new();
        let mut all_symbols = HashSet::new();
        for &entry in entries {
            let (labels, symbols) = self.search(entry);
            all_labels.extend(labels);
            all_symbols.extend(symbols);
        }
        (all_labels, all_symbols)
    }

    pub fn search(&self, entry: &'a str) -> (HashSet<&'a str>, HashSet<&'a str>) {
        if let Some(cached) = self._cache.borrow().get(entry) {
            return cached.clone();
        }

        let mut used_labels = HashSet::new();
        let mut used_symbols = HashSet::new();
        let mut visited = HashSet::new();
        let mut worklist = vec![entry];
        visited.insert(entry);

        while let Some(current) = worklist.pop() {
            if let Some((labels, symbols)) = self.graph.get(current) {
                for &label in labels {
                    used_labels.insert(label);
                    if visited.insert(label) {
                        worklist.push(label);
                    }
                }
                for &sym in symbols {
                    used_symbols.insert(sym);
                }
            }
        }

        let result = (used_labels, used_symbols);
        self._cache.borrow_mut().insert(entry, result.clone());
        result
    }

    pub fn print(&self, used: &HashSet<&'a str>) {
        println!("------------------------------------------------------------");
        for (&symbol, (labels, symbols)) in &self.graph {
            let mut labels: Vec<_> = labels.iter().copied().collect();
            let mut symbols: Vec<_> = symbols.iter().copied().collect();
            labels.sort();
            symbols.sort();
            if used.contains(symbol) {
                cprintln!(
                    "<green>✓ {}</green> -> labels: [{}], symbols: [{}]",
                    symbol,
                    labels.join(", "),
                    symbols.join(", ")
                );
            } else {
                cprintln!(
                    "<dim>✗ {}</dim> -> labels: [{}], symbols: [{}]",
                    symbol,
                    labels.join(", "),
                    symbols.join(", ")
                );
            }
        }
    }
}
