use crate::convert::Code;
use color_print::cprintln;
use indexmap::IndexMap;
use std::collections::HashSet;

// Generate symbol dependency graph
pub fn dependency<'a>(codes: &'a IndexMap<&'a str, Code>) -> IndexMap<&'a str, Vec<&'a str>> {
    let mut deps = IndexMap::new();
    for (&name, code) in codes {
        let mut refs = Vec::new();
        for (_, label) in &code.0 {
            if let Some(label) = label {
                refs.push(label.as_ref());
            }
        }
        deps.insert(name, refs);
    }
    deps
}

// Search all used symbol
pub fn search<'a>(deps: IndexMap<&'a str, Vec<&'a str>>, entry: Vec<&str>) -> HashSet<&'a str> {
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
pub fn print<'a>(deps: &IndexMap<&'a str, Vec<&'a str>>, used: &HashSet<&'a str>) {
    cprintln!("<bold>=== Dead Code Elimination Report ===</bold>");
    eprintln!();

    // IndexMap preserves insertion order, so no need to sort
    for (&symbol, deps_list) in deps {
        let is_kept = used.contains(symbol);
        let deps_str = if deps_list.is_empty() {
            "None".to_string()
        } else {
            deps_list.join(", ")
        };

        if is_kept {
            cprintln!("<green>✓ {:<20}</green> -> {}", symbol, deps_str);
        } else {
            cprintln!("<red>✗ {:<20}</red> -> {}", symbol, deps_str);
        }
    }

    eprintln!();

    // Count how many symbols from deps are actually kept
    let kept_count = deps.keys().filter(|k| used.contains(*k)).count();
    let eliminated_count = deps.len().saturating_sub(kept_count);

    cprintln!(
        "Total: {} symbols, <green>{} kept</green>, <red>{} eliminated</red>",
        deps.len(),
        kept_count,
        eliminated_count
    );
    eprintln!();
}
