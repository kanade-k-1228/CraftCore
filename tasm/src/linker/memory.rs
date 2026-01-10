use crate::error::Error;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Memory {
    range: (usize, usize),
    sections: HashMap<String, (usize, usize)>,
}

impl Memory {
    pub fn new(begin: usize, end: usize) -> Self {
        Self {
            range: (begin, end),
            sections: HashMap::new(),
        }
    }

    pub fn section(mut self, name: &str, start: usize, end: usize) -> Self {
        self.sections.insert(name.to_string(), (start, end));
        self
    }

    pub fn get(&self, name: &str) -> Result<(usize, usize), Error> {
        self.sections
            .get(name)
            .copied()
            .ok_or_else(|| Error::SectionNotFound(name.to_string()))
    }
}

impl Memory {
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }
}
