use indexmap::IndexMap;

#[derive(Debug, Clone)]
pub enum Ident {
    Code,
    Static,
    Const,
}

pub struct Idents(IndexMap<String, ((String, usize), Ident, u16)>);

impl Idents {
    pub fn new() -> Self {
        Idents(IndexMap::new())
    }

    pub fn insert(
        &mut self,
        name: String,
        location: (String, usize),
        kind: Ident,
        value: u16,
    ) -> Option<((String, usize), Ident, u16)> {
        self.0.insert(name, (location, kind, value))
    }

    pub fn get(&self, name: &str) -> Option<&((String, usize), Ident, u16)> {
        self.0.get(name)
    }

    pub fn get_val(&self, name: &str) -> Option<u16> {
        self.0.get(name).map(|(_, _, val)| *val)
    }
}
