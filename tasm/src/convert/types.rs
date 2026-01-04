use arch::inst::Inst;

#[derive(Debug, Clone)]
pub enum Immidiate {
    Literal(u16),
    Symbol(String, usize),
}

#[derive(Debug, Clone)]
pub struct Code(pub Vec<(Inst, Option<Immidiate>)>);
