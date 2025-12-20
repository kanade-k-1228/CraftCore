use arch::inst::Inst;

#[derive(Debug, Clone)]
pub struct Code(pub Vec<(Inst, Option<String>)>);
