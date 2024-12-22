use std::collections::HashMap;

use crate::parser::{Label, Line};

pub struct Labels(HashMap<String, (Line, Label, u16)>);

impl Labels {
    pub fn new() -> Self {
        Labels(HashMap::new())
    }

    pub fn insert(
        &mut self,
        name: String,
        entry: (Line, Label, u16),
    ) -> Option<(Line, Label, u16)> {
        self.0.insert(name, entry)
    }

    pub fn get(&self, name: &str) -> Option<&(Line, Label, u16)> {
        self.0.get(name)
    }
}
