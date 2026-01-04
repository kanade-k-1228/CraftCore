use crate::convert::types::{Code, Immidiate};
use color_print::cprintln;
use indexmap::IndexMap;
use std::collections::HashSet;

// Generate symbol dependency graph
pub fn dependency<'a>(codes: &'a IndexMap<&'a str, Code>) -> IndexMap<&'a str, HashSet<&'a str>> {
    let mut deps = IndexMap::new();
    for (&name, code) in codes {
        let mut refs = HashSet::new();
        for (_, label) in &code.0 {
            if let Some(Immidiate::Symbol(symbol, _)) = label {
                refs.insert(symbol.as_str());
            }
        }
        deps.insert(name, refs);
    }
    deps
}

// Search all used symbol
pub fn search<'a>(deps: IndexMap<&'a str, HashSet<&'a str>>, entry: Vec<&str>) -> HashSet<&'a str> {
    let mut used = HashSet::new();
    let mut worklist: Vec<&'a str> = Vec::new();

    for entry in entry {
        if let Some(&key) = deps.keys().find(|&&k| k == entry) {
            used.insert(key);
            worklist.push(key);
        }
    }

    while let Some(symbol) = worklist.pop() {
        if let Some(dependencies) = deps.get(symbol) {
            for &dep in dependencies {
                if used.insert(dep) {
                    worklist.push(dep);
                }
            }
        }
    }

    used
}

// Print dependency
pub fn print_deps<'a>(deps: &IndexMap<&'a str, HashSet<&'a str>>, used: &HashSet<&'a str>) {
    println!("------------------------------------------------------------");
    for (&symbol, dep) in deps {
        let mut dep: Vec<_> = dep.iter().copied().collect();
        dep.sort();
        if used.contains(symbol) {
            cprintln!("<green>✓ {}</green> -> {}", symbol, dep.join(", "));
        } else {
            cprintln!("<dim>✗ {}</dim> -> {}", symbol, dep.join(", "));
        }
    }
}
