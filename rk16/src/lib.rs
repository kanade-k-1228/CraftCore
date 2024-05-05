use std::fmt::Display;

#[derive(Debug)]
pub enum Reg {
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

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.print())
    }
}

impl Reg {
    pub fn parse(s: &str) -> Result<Reg, String> {
        match s {
            "zero" => Ok(Reg::ZERO),
            "irq" => Ok(Reg::IRQ),
            "pc" => Ok(Reg::PC),
            "sp" => Ok(Reg::SP),
            "ra" => Ok(Reg::RA),
            "fp" => Ok(Reg::FP),
            "a0" => Ok(Reg::A0),
            "a1" => Ok(Reg::A1),
            "t0" => Ok(Reg::T0),
            "t1" => Ok(Reg::T1),
            "t2" => Ok(Reg::T2),
            "t3" => Ok(Reg::T3),
            "s0" => Ok(Reg::S0),
            "s1" => Ok(Reg::S1),
            "s2" => Ok(Reg::S2),
            "s3" => Ok(Reg::S3),
            _ => Err(format!("Unknown Reg Name: {}", s)),
        }
    }
    pub fn print(&self) -> String {
        match self {
            Reg::ZERO => "zero",
            Reg::IRQ => "irq",
            Reg::PC => "pc",
            Reg::SP => "sp",
            Reg::RA => "ra",
            Reg::FP => "fp",
            Reg::A0 => "a0",
            Reg::A1 => "a1",
            Reg::T0 => "t0",
            Reg::T1 => "t1",
            Reg::T2 => "t2",
            Reg::T3 => "t3",
            Reg::S0 => "s0",
            Reg::S1 => "s1",
            Reg::S2 => "s2",
            Reg::S3 => "s3",
        }
        .to_string()
    }
}

enum Field {
    Calc(Reg, Reg, Reg, u8),
    CalcI(Reg, Reg, u16, u8),
    Load(),
    Store(),
    Ctrl(),
}

enum Arg {
    Reg,
    Imm,
}

struct Asm<const ARG_LEN: usize> {
    name: &'static str,
    args: [Arg; ARG_LEN],
    bit: Field,
}

macro_rules! asm {
    () => {
        Asm {
            name: "addi",
            args: [Arg::Reg, Arg::Reg, Arg::Reg],
            bit: Field::Calc(Reg::T0, Reg::T0, Reg::T0, 0),
        }
    };
}

const ASMS: (Asm<3>, Asm<3>, Asm<3>) = (
    Asm {
        name: "add",
        args: [Arg::Reg, Arg::Reg, Arg::Reg],
        bit: Field::Calc(Reg::T0, Reg::T0, Reg::T0, 0),
    },
    Asm {
        name: "sub",
        args: [Arg::Reg, Arg::Reg, Arg::Reg],
        bit: Field::Calc(Reg::T0, Reg::T0, Reg::T0, 0),
    },
    asm!(),
);
