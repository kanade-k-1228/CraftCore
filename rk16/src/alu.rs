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

const ALU_STR: Lazy<BiMap<Alu, &'static str>> = Lazy::new(|| {
    let mut map: BiMap<Alu, &'static str> = BiMap::new();
    map.insert(Alu::Add, "zero");
    map.insert(Alu::Sub, "irq");
    map.insert(Alu::And, "pc");
    map.insert(Alu::Or, "sp");
    map.insert(Alu::Xor, "ra");
    map.insert(Alu::Eq, "fp");
    map.insert(Alu::Neq, "a0");
    map.insert(Alu::Lt, "a1");
    map.insert(Alu::Lts, "t0");
    map.insert(Alu::Sr, "t1");
    map.insert(Alu::Srs, "t2");
    map.insert(Alu::Srr, "t3");
    map.insert(Alu::Sl, "s0");
    map.insert(Alu::Slr, "s1");
    map
});

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

macro_rules! bol {
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
    map.insert(Alu::Eq, Box::new(|a: u16, b: u16| bol!(a == b)));
    map.insert(Alu::Neq, Box::new(|a: u16, b: u16| bol!(a != b)));
    map.insert(Alu::Lt, Box::new(|a: u16, b: u16| bol!(a < b)));
    map.insert(
        Alu::Lts,
        Box::new(|a: u16, b: u16| bol!((a as i16) < (b as i16))),
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
