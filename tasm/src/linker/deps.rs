use crate::convert::Code;
use std::collections::{HashMap, HashSet};

pub fn dependency<'a>(codes: &'a HashMap<&'a str, Code>) -> HashMap<&'a str, Vec<&'a str>> {
    let mut deps = HashMap::new();
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

pub fn filter<'a>(deps: HashMap<&'a str, Vec<&'a str>>, entry: Vec<&str>) -> HashSet<&'a str> {
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
