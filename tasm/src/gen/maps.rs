use bimap::BiMap;
use crate::symbols::Symbols;
use serde::{Serialize, Deserialize};
use std::collections::BTreeMap;

#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionMapEntry {
    pub address: usize,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct StaticMapEntry {
    pub address: usize,
    pub size: usize,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DataMapEntry {
    pub address: usize,
}

/// Generate function map (name -> address mapping for functions and asm blocks)
pub fn generate_function_map(_symbols: &Symbols, imap: &BiMap<String, usize>) -> String {
    let mut map: BTreeMap<String, FunctionMapEntry> = BTreeMap::new();

    for (name, addr) in imap.iter() {
        map.insert(name.clone(), FunctionMapEntry { address: *addr });
    }

    serde_yaml::to_string(&map).unwrap_or_else(|e| format!("# Error generating YAML: {}", e))
}

/// Generate static map (name -> address mapping for static variables)
pub fn generate_static_map(symbols: &Symbols, dmap: &BiMap<String, usize>) -> String {
    let mut map: BTreeMap<String, StaticMapEntry> = BTreeMap::new();

    for (&name, (ty, _, _)) in symbols.statics.0.iter() {
        if let Some(&addr) = dmap.get_by_left(name) {
            map.insert(
                name.to_string(),
                StaticMapEntry {
                    address: addr,
                    size: ty.sizeof(),
                },
            );
        }
    }

    serde_yaml::to_string(&map).unwrap_or_else(|e| format!("# Error generating YAML: {}", e))
}

/// Generate data map (name -> address mapping for all data including constants)
pub fn generate_data_map(dmap: &BiMap<String, usize>) -> String {
    let mut map: BTreeMap<String, DataMapEntry> = BTreeMap::new();

    for (name, addr) in dmap.iter() {
        map.insert(name.clone(), DataMapEntry { address: *addr });
    }

    serde_yaml::to_string(&map).unwrap_or_else(|e| format!("# Error generating YAML: {}", e))
}
