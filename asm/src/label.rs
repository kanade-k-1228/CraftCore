pub struct Labels(std::collections::HashMap<String, crate::parser::Line>);

impl Labels {
    pub fn new() -> Self {
        Labels(std::collections::HashMap::new())
    }

    pub fn insert(
        &mut self,
        name: String,
        entry: crate::parser::Line,
    ) -> Option<crate::parser::Line> {
        self.0.insert(name, entry)
    }

    pub fn get(&self, name: &str) -> Option<&crate::parser::Line> {
        self.0.get(name)
    }

    pub fn get_val(&self, name: &str) -> Option<u16> {
        self.0
            .get(name)
            .and_then(|line| line.get_label())
            .map(|lab| lab.get_val())
    }
}
