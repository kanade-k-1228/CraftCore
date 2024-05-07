use std::collections::HashMap;

use bimap::BiMap;
use once_cell::sync::Lazy;

#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy)]
pub enum OpKind {
    Add,
    Sub,
    And,
    Or,
    Xor,
    Eq,
    Neq,
    Lt,
    Lts,
    Sr,
    Srs,
    Srr,
    Sl,
    Slr,
    #[default]
    Nop,
    Mov,
    Addi,
    Subi,
    Andi,
    Ori,
    Xori,
    Eqi,
    Neqi,
    Lti,
    Ltsi,
    Not,
    Loadi,
    Load,
    Store,
    If,
    Ifr,
    Jump,
    Jumpr,
    Call,
    Ret,
    Iret,
}

const OP_STR: Lazy<BiMap<OpKind, &'static str>> = Lazy::new(|| {
    let mut map: BiMap<OpKind, &'static str> = BiMap::new();
    map.insert(OpKind::Add, "add");
    map.insert(OpKind::Sub, "sub");
    map.insert(OpKind::And, "and");
    map.insert(OpKind::Or, "or");
    map.insert(OpKind::Xor, "xor");
    map.insert(OpKind::Eq, "eq");
    map.insert(OpKind::Neq, "neq");
    map.insert(OpKind::Lt, "lt");
    map.insert(OpKind::Lts, "lts");
    map.insert(OpKind::Sr, "sr");
    map.insert(OpKind::Srs, "srs");
    map.insert(OpKind::Srr, "srr");
    map.insert(OpKind::Sl, "sl");
    map.insert(OpKind::Slr, "slr");
    map.insert(OpKind::Nop, "nop");
    map.insert(OpKind::Mov, "mov");
    map.insert(OpKind::Addi, "addi");
    map.insert(OpKind::Subi, "subi");
    map.insert(OpKind::Andi, "andi");
    map.insert(OpKind::Ori, "ori");
    map.insert(OpKind::Xori, "xori");
    map.insert(OpKind::Eqi, "eqi");
    map.insert(OpKind::Neqi, "neqi");
    map.insert(OpKind::Lti, "lti");
    map.insert(OpKind::Ltsi, "ltsi");
    map.insert(OpKind::Not, "not");
    map.insert(OpKind::Loadi, "loadi");
    map.insert(OpKind::Load, "load");
    map.insert(OpKind::Store, "store");
    map.insert(OpKind::If, "if");
    map.insert(OpKind::Ifr, "ifr");
    map.insert(OpKind::Jump, "jump");
    map.insert(OpKind::Jumpr, "jumpr");
    map.insert(OpKind::Call, "call");
    map.insert(OpKind::Ret, "ret");
    map.insert(OpKind::Iret, "iret");
    map
});

impl OpKind {
    pub fn parse(s: &str) -> Result<OpKind, String> {
        if let Some(mnemonic) = OP_STR.get_by_right(s) {
            Ok(*mnemonic)
        } else {
            Err(format!("Unknown Operation Name: `{}`", s))
        }
    }
    pub fn format(&self) -> String {
        OP_STR.get_by_left(self).unwrap().to_string()
    }
}

enum Arg {
    Rd,
    Rs1,
    Rs2,
    Imm,
}

const OP_ARGS: Lazy<HashMap<OpKind, Vec<Arg>>> = Lazy::new(|| {
    let mut map: HashMap<OpKind, Vec<Arg>> = HashMap::new();
    map.insert(OpKind::Add, vec![Arg::Rd, Arg::Rs1, Arg::Rs2]);
    map.insert(OpKind::Sub, vec![Arg::Rd, Arg::Rs1, Arg::Rs2]);
    map.insert(OpKind::And, vec![Arg::Rd, Arg::Rs1, Arg::Rs2]);
    map.insert(OpKind::Or, vec![Arg::Rd, Arg::Rs1, Arg::Rs2]);
    map.insert(OpKind::Xor, vec![Arg::Rd, Arg::Rs1, Arg::Rs2]);
    map.insert(OpKind::Eq, vec![Arg::Rd, Arg::Rs1, Arg::Rs2]);
    map.insert(OpKind::Neq, vec![Arg::Rd, Arg::Rs1, Arg::Rs2]);
    map.insert(OpKind::Lt, vec![Arg::Rd, Arg::Rs1, Arg::Rs2]);
    map.insert(OpKind::Lts, vec![Arg::Rd, Arg::Rs1, Arg::Rs2]);
    map.insert(OpKind::Sr, vec![Arg::Rd, Arg::Rs1]);
    map.insert(OpKind::Srs, vec![Arg::Rd, Arg::Rs1]);
    map.insert(OpKind::Srr, vec![Arg::Rd, Arg::Rs1]);
    map.insert(OpKind::Sl, vec![Arg::Rd, Arg::Rs1]);
    map.insert(OpKind::Slr, vec![Arg::Rd, Arg::Rs1]);
    map.insert(OpKind::Nop, vec![]);
    map.insert(OpKind::Mov, vec![Arg::Rd, Arg::Rs1]);
    map.insert(OpKind::Addi, vec![Arg::Rd, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::Subi, vec![Arg::Rd, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::Andi, vec![Arg::Rd, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::Ori, vec![Arg::Rd, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::Xori, vec![Arg::Rd, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::Eqi, vec![Arg::Rd, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::Neqi, vec![Arg::Rd, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::Lti, vec![Arg::Rd, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::Ltsi, vec![Arg::Rd, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::Not, vec![Arg::Rd, Arg::Rs1]);
    map.insert(OpKind::Loadi, vec![Arg::Rd, Arg::Imm]);
    map.insert(OpKind::Load, vec![Arg::Rd, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::Store, vec![Arg::Rs2, Arg::Rs1, Arg::Imm]);
    map.insert(OpKind::If, vec![Arg::Rs2, Arg::Imm]);
    map.insert(OpKind::Ifr, vec![Arg::Rs2, Arg::Imm]);
    map.insert(OpKind::Jump, vec![Arg::Imm]);
    map.insert(OpKind::Jumpr, vec![Arg::Imm]);
    map.insert(OpKind::Call, vec![Arg::Imm]);
    map.insert(OpKind::Ret, vec![]);
    map.insert(OpKind::Iret, vec![]);
    map
});

const OP_FIELD: Lazy<HashMap<OpKind, Vec<(usize)>>> = Lazy::new(|| {
    let mut map: HashMap<OpKind, Vec<(usize)>> = HashMap::new();
    map.insert(OpKind::Add, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Sub, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::And, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Or, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Xor, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Eq, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Neq, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Lt, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Lts, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Sr, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Srs, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Srr, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Sl, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Slr, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Nop, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Mov, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Addi, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Subi, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Andi, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Ori, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Xori, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Eqi, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Neqi, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Lti, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Ltsi, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Not, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Loadi, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Load, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Store, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::If, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Ifr, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Jump, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Jumpr, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Call, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Ret, vec![(16), (4), (4), (4), (4)]);
    map.insert(OpKind::Iret, vec![(16), (4), (4), (4), (4)]);
    map
});
