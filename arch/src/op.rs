use crate::{alu::ALU, reg::Reg};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op<R, I> {
    CALC(ALU, R, R, R),
    CALCI(ALU, R, R, I),
    LOAD(R, R, I),
    STORE(R, R, I),
    CTRL(R, R, R, I),
}

// ----------------------------------------------------------------------------

impl<R0, I0> Op<R0, I0> {
    pub fn resolve<I1>(self, f: impl Fn(I0) -> I1) -> Op<R0, I1> {
        match self {
            Op::CALC(alu, rd, rs1, rs2) => Op::CALC(alu, rd, rs1, rs2),
            Op::CALCI(alu, rd, rs, imm) => Op::CALCI(alu, rd, rs, f(imm)),
            Op::LOAD(rd, rs, imm) => Op::LOAD(rd, rs, f(imm)),
            Op::STORE(rs2, rs1, imm) => Op::STORE(rs2, rs1, f(imm)),
            Op::CTRL(rd, rs1, rs2, imm) => Op::CTRL(rd, rs1, rs2, f(imm)),
        }
    }

    pub fn imm(&self) -> Option<&I0> {
        match self {
            Op::CALCI(_, _, _, imm)
            | Op::LOAD(_, _, imm)
            | Op::STORE(_, _, imm)
            | Op::CTRL(_, _, _, imm) => Some(imm),
            Op::CALC(..) => None,
        }
    }
}

// ----------------------------------------------------------------------------

const fn pack(hi: u8, lo: u8) -> u8 {
    ((hi & 0xF) << 4) | (lo & 0xF)
}

const fn unpack(byte: u8) -> (u8, u8) {
    (byte >> 4, byte & 0xF)
}

impl Op<Reg, u16> {
    const OP_CALC: u8 = 0b0000;
    const OP_CALCI: u8 = 0b0001;
    const OP_LOAD: u8 = 0b0011;
    const OP_STORE: u8 = 0b0111;
    const OP_CTRL: u8 = 0b1111;

    pub fn encode(&self) -> u32 {
        let (c, rd, rs1, rs2, imm): (u8, u8, u8, u8, u16) = match *self {
            Op::CALC(a, rd, rs1, rs2) => (Self::OP_CALC, rd as u8, rs1 as u8, rs2 as u8, a as u16),
            Op::CALCI(a, rd, rs1, imm) => (Self::OP_CALCI, rd as u8, rs1 as u8, a as u8, imm),
            Op::LOAD(rd, rs1, imm) => (Self::OP_LOAD, rd as u8, rs1 as u8, 0, imm),
            Op::STORE(rs2, rs1, imm) => (Self::OP_STORE, 0, rs1 as u8, rs2 as u8, imm),
            Op::CTRL(rd, rs1, rs2, imm) => (Self::OP_CTRL, rd as u8, rs1 as u8, rs2 as u8, imm),
        };
        let [b2, b3] = imm.to_le_bytes();
        u32::from_le_bytes([pack(rs1, c), pack(rd, rs2), b2, b3])
    }

    pub fn decode(bin: u32) -> Op<Reg, u16> {
        let [b0, b1, b2, b3] = bin.to_le_bytes();
        let (rs1, c) = unpack(b0);
        let (rd, rs2) = unpack(b1);
        let imm = u16::from_le_bytes([b2, b3]);
        match c {
            Self::OP_CALC => Op::CALC(
                ALU::from(imm as u8),
                Reg::from(rd),
                Reg::from(rs1),
                Reg::from(rs2),
            ),
            Self::OP_CALCI => Op::CALCI(ALU::from(rs2), Reg::from(rd), Reg::from(rs1), imm),
            Self::OP_LOAD => Op::LOAD(Reg::from(rd), Reg::from(rs1), imm),
            Self::OP_STORE => Op::STORE(Reg::from(rs2), Reg::from(rs1), imm),
            Self::OP_CTRL => Op::CTRL(Reg::from(rd), Reg::from(rs1), Reg::from(rs2), imm),
            _ => panic!("Unknown opcode"),
        }
    }
}

#[test]
fn test_op_all() {
    // CALC: alu / rd / rs1 / rs2 を全探索
    for alu in 0u8..=13 {
        for rd in 0u8..=0xF {
            for rs1 in 0u8..=0xF {
                for rs2 in 0u8..=0xF {
                    let op = Op::CALC(
                        ALU::from(alu),
                        Reg::from(rd),
                        Reg::from(rs1),
                        Reg::from(rs2),
                    );
                    assert_eq!(op, Op::decode(op.encode()));
                }
            }
        }
    }
    // CALCI: alu / rd / rs1 / imm を全探索
    for alu in 0u8..=13 {
        for rd in 0u8..=0xF {
            for rs1 in 0u8..=0xF {
                for imm in 0u16..=0xFFFF {
                    let op = Op::CALCI(ALU::from(alu), Reg::from(rd), Reg::from(rs1), imm);
                    assert_eq!(op, Op::decode(op.encode()));
                }
            }
        }
    }
    // LOAD: rd / rs1 / imm を全探索
    for rd in 0u8..=0xF {
        for rs1 in 0u8..=0xF {
            for imm in 0u16..=0xFFFF {
                let op = Op::LOAD(Reg::from(rd), Reg::from(rs1), imm);
                assert_eq!(op, Op::decode(op.encode()));
            }
        }
    }
    // STORE: rs2 / rs1 / imm を全探索
    for rs2 in 0u8..=0xF {
        for rs1 in 0u8..=0xF {
            for imm in 0u16..=0xFFFF {
                let op = Op::STORE(Reg::from(rs2), Reg::from(rs1), imm);
                assert_eq!(op, Op::decode(op.encode()));
            }
        }
    }
    // CTRL: rd / rs1 / rs2 / imm を全探索
    for rd in 0u8..=0xF {
        for rs1 in 0u8..=0xF {
            for rs2 in 0u8..=0xF {
                for imm in 0u16..=0xFFFF {
                    let op = Op::CTRL(Reg::from(rd), Reg::from(rs1), Reg::from(rs2), imm);
                    assert_eq!(op, Op::decode(op.encode()));
                }
            }
        }
    }
}

// ----------------------------------------------------------------------------

impl<I: From<u16>> Op<Reg, I> {
    // Arithmetic
    pub fn add(rd: Reg, rs1: Reg, rs2: Reg) -> Self {
        Op::CALC(ALU::ADD, rd, rs1, rs2)
    }
    pub fn addi(rd: Reg, rs: Reg, imm: I) -> Self {
        Op::CALCI(ALU::ADD, rd, rs, imm)
    }
    pub fn sub(rd: Reg, rs1: Reg, rs2: Reg) -> Self {
        Op::CALC(ALU::SUB, rd, rs1, rs2)
    }
    pub fn subi(rd: Reg, rs: Reg, imm: I) -> Self {
        Op::CALCI(ALU::SUB, rd, rs, imm)
    }

    // Logical
    pub fn not(rd: Reg, rs: Reg) -> Self {
        Op::CALCI(ALU::XOR, rd, rs, I::from(0xFFFF))
    }
    pub fn and(rd: Reg, rs1: Reg, rs2: Reg) -> Self {
        Op::CALC(ALU::AND, rd, rs1, rs2)
    }
    pub fn andi(rd: Reg, rs: Reg, imm: I) -> Self {
        Op::CALCI(ALU::AND, rd, rs, imm)
    }
    pub fn or(rd: Reg, rs1: Reg, rs2: Reg) -> Self {
        Op::CALC(ALU::OR, rd, rs1, rs2)
    }
    pub fn ori(rd: Reg, rs: Reg, imm: I) -> Self {
        Op::CALCI(ALU::OR, rd, rs, imm)
    }
    pub fn xor(rd: Reg, rs1: Reg, rs2: Reg) -> Self {
        Op::CALC(ALU::XOR, rd, rs1, rs2)
    }
    pub fn xori(rd: Reg, rs: Reg, imm: I) -> Self {
        Op::CALCI(ALU::XOR, rd, rs, imm)
    }

    // Comparison
    pub fn eq(rd: Reg, rs1: Reg, rs2: Reg) -> Self {
        Op::CALC(ALU::EQ, rd, rs1, rs2)
    }
    pub fn eqi(rd: Reg, rs: Reg, imm: I) -> Self {
        Op::CALCI(ALU::EQ, rd, rs, imm)
    }
    pub fn neq(rd: Reg, rs1: Reg, rs2: Reg) -> Self {
        Op::CALC(ALU::NEQ, rd, rs1, rs2)
    }
    pub fn neqi(rd: Reg, rs: Reg, imm: I) -> Self {
        Op::CALCI(ALU::NEQ, rd, rs, imm)
    }
    pub fn lt(rd: Reg, rs1: Reg, rs2: Reg) -> Self {
        Op::CALC(ALU::LT, rd, rs1, rs2)
    }
    pub fn lti(rd: Reg, rs: Reg, imm: I) -> Self {
        Op::CALCI(ALU::LT, rd, rs, imm)
    }
    pub fn lts(rd: Reg, rs1: Reg, rs2: Reg) -> Self {
        Op::CALC(ALU::LTS, rd, rs1, rs2)
    }
    pub fn ltsi(rd: Reg, rs: Reg, imm: I) -> Self {
        Op::CALCI(ALU::LTS, rd, rs, imm)
    }

    // Shift
    pub fn sr(rd: Reg, rs: Reg) -> Self {
        Op::CALC(ALU::SR, rd, rs, Reg::Z)
    }
    pub fn srs(rd: Reg, rs: Reg) -> Self {
        Op::CALC(ALU::SRS, rd, rs, Reg::Z)
    }
    pub fn srr(rd: Reg, rs: Reg) -> Self {
        Op::CALC(ALU::SRR, rd, rs, Reg::Z)
    }
    pub fn sl(rd: Reg, rs: Reg) -> Self {
        Op::CALC(ALU::SL, rd, rs, Reg::Z)
    }
    pub fn slr(rd: Reg, rs: Reg) -> Self {
        Op::CALC(ALU::SLR, rd, rs, Reg::Z)
    }

    // Move / Memory
    pub fn mov(rd: Reg, rs: Reg) -> Self {
        Op::CALC(ALU::ADD, rd, rs, Reg::Z)
    }
    pub fn load(rd: Reg, rs: Reg, imm: I) -> Self {
        Op::LOAD(rd, rs, imm)
    }
    pub fn loadi(rd: Reg, imm: I) -> Self {
        Op::CALCI(ALU::ADD, rd, Reg::Z, imm)
    }
    pub fn store(rs2: Reg, rs1: Reg, imm: I) -> Self {
        Op::STORE(rs2, rs1, imm)
    }

    // Control
    pub fn nop() -> Self {
        Op::CALC(ALU::ADD, Reg::Z, Reg::Z, Reg::Z)
    }
    pub fn jump(imm: I) -> Self {
        Op::CTRL(Reg::Z, Reg::Z, Reg::Z, imm)
    }
    pub fn jumpr(imm: I) -> Self {
        Op::CTRL(Reg::Z, Reg::PC, Reg::Z, imm)
    }
    pub fn jumpif(rs: Reg, imm: I) -> Self {
        Op::CTRL(Reg::Z, Reg::Z, rs, imm)
    }
    pub fn jumpifr(rs: Reg, imm: I) -> Self {
        Op::CTRL(Reg::Z, Reg::PC, rs, imm)
    }
    pub fn call(imm: I) -> Self {
        Op::CTRL(Reg::RA, Reg::Z, Reg::Z, imm)
    }
    pub fn callr(reg: Reg) -> Self {
        Op::CTRL(Reg::RA, reg, Reg::Z, I::from(0))
    }
    pub fn ret() -> Self {
        Op::CTRL(Reg::Z, Reg::RA, Reg::Z, I::from(0))
    }
    pub fn iret() -> Self {
        Op::CTRL(Reg::Z, Reg::IRA, Reg::Z, I::from(0))
    }
}

// ----------------------------------------------------------------------------

impl<I: Display> Op<Reg, I> {
    pub fn print(&self) -> String {
        match self {
            // CALC: nop / mov / arith / cmp / shift
            Op::CALC(ALU::ADD, Reg::Z, Reg::Z, Reg::Z) => "nop()".to_string(),
            Op::CALC(ALU::ADD, rd, rs, Reg::Z) => format!("mov({}, {})", rd, rs),
            Op::CALC(ALU::ADD, rd, rs1, rs2) => format!("add({}, {}, {})", rd, rs1, rs2),
            Op::CALC(ALU::SUB, rd, rs1, rs2) => format!("sub({}, {}, {})", rd, rs1, rs2),
            Op::CALC(ALU::AND, rd, rs1, rs2) => format!("and({}, {}, {})", rd, rs1, rs2),
            Op::CALC(ALU::OR, rd, rs1, rs2) => format!("or({}, {}, {})", rd, rs1, rs2),
            Op::CALC(ALU::XOR, rd, rs1, rs2) => format!("xor({}, {}, {})", rd, rs1, rs2),
            Op::CALC(ALU::EQ, rd, rs1, rs2) => format!("eq({}, {}, {})", rd, rs1, rs2),
            Op::CALC(ALU::NEQ, rd, rs1, rs2) => format!("neq({}, {}, {})", rd, rs1, rs2),
            Op::CALC(ALU::LT, rd, rs1, rs2) => format!("lt({}, {}, {})", rd, rs1, rs2),
            Op::CALC(ALU::LTS, rd, rs1, rs2) => format!("lts({}, {}, {})", rd, rs1, rs2),
            Op::CALC(ALU::SR, rd, rs, _) => format!("sr({}, {})", rd, rs),
            Op::CALC(ALU::SRS, rd, rs, _) => format!("srs({}, {})", rd, rs),
            Op::CALC(ALU::SRR, rd, rs, _) => format!("srr({}, {})", rd, rs),
            Op::CALC(ALU::SL, rd, rs, _) => format!("sl({}, {})", rd, rs),
            Op::CALC(ALU::SLR, rd, rs, _) => format!("slr({}, {})", rd, rs),

            // CALCI: loadi / immediates
            Op::CALCI(ALU::ADD, rd, Reg::Z, imm) => format!("loadi({}, {})", rd, imm),
            Op::CALCI(ALU::ADD, rd, rs, imm) => format!("addi({}, {}, {})", rd, rs, imm),
            Op::CALCI(ALU::SUB, rd, rs, imm) => format!("subi({}, {}, {})", rd, rs, imm),
            Op::CALCI(ALU::AND, rd, rs, imm) => format!("andi({}, {}, {})", rd, rs, imm),
            Op::CALCI(ALU::OR, rd, rs, imm) => format!("ori({}, {}, {})", rd, rs, imm),
            Op::CALCI(ALU::XOR, rd, rs, imm) => format!("xori({}, {}, {})", rd, rs, imm),
            Op::CALCI(ALU::EQ, rd, rs, imm) => format!("eqi({}, {}, {})", rd, rs, imm),
            Op::CALCI(ALU::NEQ, rd, rs, imm) => format!("neqi({}, {}, {})", rd, rs, imm),
            Op::CALCI(ALU::LT, rd, rs, imm) => format!("lti({}, {}, {})", rd, rs, imm),
            Op::CALCI(ALU::LTS, rd, rs, imm) => format!("ltsi({}, {}, {})", rd, rs, imm),
            Op::CALCI(alu, rd, rs, imm) => format!("calci({:?}, {}, {}, {})", alu, rd, rs, imm),

            // LOAD / STORE
            Op::LOAD(rd, rs, imm) => format!("load({}, {}, {})", rd, rs, imm),
            Op::STORE(rs2, rs1, imm) => format!("store({}, {}, {})", rs2, rs1, imm),

            // CTRL: jump / call / ret / iret
            Op::CTRL(Reg::Z, Reg::RA, Reg::Z, _) => "ret()".to_string(),
            Op::CTRL(Reg::Z, Reg::IRA, Reg::Z, _) => "iret()".to_string(),
            Op::CTRL(Reg::Z, Reg::Z, Reg::Z, imm) => format!("jump({})", imm),
            Op::CTRL(Reg::Z, Reg::PC, Reg::Z, imm) => format!("jumpr({})", imm),
            Op::CTRL(Reg::Z, Reg::Z, rs, imm) => format!("jumpif({}, {})", rs, imm),
            Op::CTRL(Reg::Z, Reg::PC, rs, imm) => format!("jumpifr({}, {})", rs, imm),
            Op::CTRL(Reg::RA, Reg::Z, Reg::Z, imm) => format!("call({})", imm),
            Op::CTRL(Reg::RA, reg, Reg::Z, _) => format!("callr({})", reg),
            Op::CTRL(rd, rs1, rs2, imm) => {
                format!("ctrl({}, {}, {}, {})", rd, rs1, rs2, imm)
            }
        }
    }
}
