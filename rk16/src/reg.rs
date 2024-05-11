use bimap::BiMap;
use once_cell::sync::Lazy;
use std::fmt::Display;

#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Reg {
    #[default]
    ZERO,
    IRQ,
    PC,
    SP,
    RA,
    FP,
    A0,
    A1,
    T0,
    T1,
    T2,
    T3,
    S0,
    S1,
    S2,
    S3,
}

const REG_STR: Lazy<BiMap<Reg, &'static str>> = Lazy::new(|| {
    let mut map: BiMap<Reg, &'static str> = BiMap::new();
    map.insert(Reg::ZERO, "zero");
    map.insert(Reg::IRQ, "irq");
    map.insert(Reg::PC, "pc");
    map.insert(Reg::SP, "sp");
    map.insert(Reg::RA, "ra");
    map.insert(Reg::FP, "fp");
    map.insert(Reg::A0, "a0");
    map.insert(Reg::A1, "a1");
    map.insert(Reg::T0, "t0");
    map.insert(Reg::T1, "t1");
    map.insert(Reg::T2, "t2");
    map.insert(Reg::T3, "t3");
    map.insert(Reg::S0, "s0");
    map.insert(Reg::S1, "s1");
    map.insert(Reg::S2, "s2");
    map.insert(Reg::S3, "s3");
    map
});

impl Reg {
    pub fn parse(s: &str) -> Result<Reg, String> {
        if let Some(reg) = REG_STR.get_by_right(s) {
            Ok(*reg)
        } else {
            Err(format!("Unknown Registor Name: `{}`", s))
        }
    }
    pub fn format(&self) -> String {
        REG_STR.get_by_left(self).unwrap().to_string()
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.format())
    }
}

const REG_BIN: Lazy<BiMap<Reg, u8>> = Lazy::new(|| {
    let mut map: BiMap<Reg, u8> = BiMap::new();
    map.insert(Reg::ZERO, 0);
    map.insert(Reg::IRQ, 1);
    map.insert(Reg::PC, 2);
    map.insert(Reg::SP, 3);
    map.insert(Reg::RA, 4);
    map.insert(Reg::FP, 5);
    map.insert(Reg::A0, 6);
    map.insert(Reg::A1, 7);
    map.insert(Reg::T0, 8);
    map.insert(Reg::T1, 9);
    map.insert(Reg::T2, 10);
    map.insert(Reg::T3, 11);
    map.insert(Reg::S0, 12);
    map.insert(Reg::S1, 13);
    map.insert(Reg::S2, 14);
    map.insert(Reg::S3, 15);
    map
});

impl Into<u8> for Reg {
    fn into(self) -> u8 {
        REG_BIN.get_by_left(&self).unwrap().clone()
    }
}
