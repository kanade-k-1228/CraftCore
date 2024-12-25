use std::collections::HashMap;

use crate::parser::{Label, Line};

pub struct Labels(HashMap<String, Line>);

impl Labels {
    pub fn new() -> Self {
        Labels(HashMap::new())
    }

    pub fn insert(&mut self, name: String, entry: Line) -> Option<Line> {
        self.0.insert(name, entry)
    }

    pub fn get(&self, name: &str) -> Option<&Line> {
        self.0.get(name)
    }

    pub fn get_val(&self, name: &str) -> Option<u16> {
        self.0
            .get(name)
            .and_then(|line| line.get_label())
            .map(|lab| lab.get_val())
    }
}
