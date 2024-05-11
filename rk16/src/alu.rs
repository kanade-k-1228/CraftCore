use bimap::BiMap;
use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Alu {
    #[default]
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
}

const ALU_BIN: Lazy<BiMap<Alu, u8>> = Lazy::new(|| {
    let mut map = BiMap::new();
    map.insert(Alu::Add, 0);
    map.insert(Alu::Sub, 1);
    map.insert(Alu::And, 2);
    map.insert(Alu::Or, 3);
    map.insert(Alu::Xor, 4);
    map.insert(Alu::Eq, 5);
    map.insert(Alu::Neq, 6);
    map.insert(Alu::Lt, 7);
    map.insert(Alu::Lts, 8);
    map.insert(Alu::Sr, 9);
    map.insert(Alu::Srs, 10);
    map.insert(Alu::Srr, 11);
    map.insert(Alu::Sl, 12);
    map.insert(Alu::Slr, 13);
    map
});

impl Into<u8> for Alu {
    fn into(self) -> u8 {
        ALU_BIN.get_by_left(&self).unwrap().clone()
    }
}
impl Into<u16> for Alu {
    fn into(self) -> u16 {
        ALU_BIN.get_by_left(&self).unwrap().clone() as u16
    }
}

impl From<u8> for Alu {
    fn from(bin: u8) -> Self {
        ALU_BIN.get_by_right(&bin).unwrap().clone()
    }
}

impl From<u16> for Alu {
    fn from(bin: u16) -> Self {
        ALU_BIN.get_by_right(&(bin as u8)).unwrap().clone()
    }
}

impl Alu {
    pub fn parse_bin(bin: u8) -> Option<Self> {
        match ALU_BIN.get_by_right(&bin) {
            Some(alu) => Some(*alu),
            None => None,
        }
    }
}

macro_rules! boo {
    ($cond:expr) => {
        if $cond {
            0xFFFF
        } else {
            0x0000
        }
    };
}

const ALU_FN: Lazy<HashMap<Alu, Box<dyn Fn(u16, u16) -> u16>>> = Lazy::new(|| {
    let mut map: HashMap<Alu, Box<dyn Fn(u16, u16) -> u16>> = HashMap::new();
    map.insert(Alu::Add, Box::new(|a: u16, b: u16| a + b));
    map.insert(Alu::Sub, Box::new(|a: u16, b: u16| a - b));
    map.insert(Alu::And, Box::new(|a: u16, b: u16| a & b));
    map.insert(Alu::Or, Box::new(|a: u16, b: u16| a | b));
    map.insert(Alu::Xor, Box::new(|a: u16, b: u16| a ^ b));
    map.insert(Alu::Eq, Box::new(|a: u16, b: u16| boo!(a == b)));
    map.insert(Alu::Neq, Box::new(|a: u16, b: u16| boo!(a != b)));
    map.insert(Alu::Lt, Box::new(|a: u16, b: u16| boo!(a < b)));
    map.insert(
        Alu::Lts,
        Box::new(|a: u16, b: u16| boo!((a as i16) < (b as i16))),
    );
    map.insert(Alu::Sr, Box::new(|a: u16, _b: u16| a >> 1));
    map.insert(Alu::Srs, Box::new(|a: u16, _b: u16| (a as i16 >> 1) as u16));
    map.insert(Alu::Srr, Box::new(|a: u16, _b: u16| a >> 1 | a << 15));
    map.insert(Alu::Sl, Box::new(|a: u16, _b: u16| a << 1));
    map.insert(Alu::Slr, Box::new(|a: u16, _b: u16| a << 1 | a >> 15));
    map
});

pub fn alu(op: Alu, a: u16, b: u16) -> u16 {
    ALU_FN.get(&op).unwrap()(a, b)
}
