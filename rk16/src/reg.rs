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

const REG_MAP: Lazy<BiMap<&'static str, Reg>> = Lazy::new(|| {
    let mut map: BiMap<&'static str, Reg> = BiMap::new();
    map.insert("zero", Reg::ZERO);
    map.insert("irq", Reg::IRQ);
    map.insert("pc", Reg::PC);
    map.insert("sp", Reg::SP);
    map.insert("ra", Reg::RA);
    map.insert("fp", Reg::FP);
    map.insert("a0", Reg::A0);
    map.insert("a1", Reg::A1);
    map.insert("t0", Reg::T0);
    map.insert("t1", Reg::T1);
    map.insert("t2", Reg::T2);
    map.insert("t3", Reg::T3);
    map.insert("s0", Reg::S0);
    map.insert("s1", Reg::S1);
    map.insert("s2", Reg::S2);
    map.insert("s3", Reg::S3);
    map
});

impl Reg {
    pub fn parse(s: &str) -> Result<Reg, String> {
        if let Some(reg) = REG_MAP.get_by_left(s) {
            Ok(*reg)
        } else {
            Err(format!("Unknown Registor Name: `{}`", s))
        }
    }
    pub fn format(&self) -> String {
        REG_MAP.get_by_right(self).unwrap().to_string()
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.format())
    }
}
