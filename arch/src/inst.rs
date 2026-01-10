use crate::{alu::ALU, op::Op, reg::Reg};

use color_print::cformat;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inst<R, I> {
    ADD(R, R, R),
    ADDI(R, R, I),
    SUB(R, R, R),
    SUBI(R, R, I),

    NOT(R, R),
    AND(R, R, R),
    ANDI(R, R, I),
    OR(R, R, R),
    ORI(R, R, I),
    XOR(R, R, R),
    XORI(R, R, I),

    EQ(R, R, R),
    EQI(R, R, I),
    NEQ(R, R, R),
    NEQI(R, R, I),
    LT(R, R, R),
    LTI(R, R, I),
    LTS(R, R, R),
    LTSI(R, R, I),

    SR(R, R),
    SRS(R, R),
    SRR(R, R),
    SL(R, R),
    SLR(R, R),

    MOV(R, R),
    LOAD(R, R, I),
    LOADI(R, I),
    STORE(R, R, I),

    NOP(),
    JUMP(I),
    JUMPR(I),
    JUMPIF(R, I),
    JUMPIFR(R, I),
    CALL(I),
    RET(),
    IRET(),
}

impl<R, I> Inst<R, I> {
    /// Returns a reference to the immediate value if this instruction has one
    pub fn imm(&self) -> Option<&I> {
        match self {
            Inst::ADDI(_, _, imm)
            | Inst::SUBI(_, _, imm)
            | Inst::ANDI(_, _, imm)
            | Inst::ORI(_, _, imm)
            | Inst::XORI(_, _, imm)
            | Inst::EQI(_, _, imm)
            | Inst::NEQI(_, _, imm)
            | Inst::LTI(_, _, imm)
            | Inst::LTSI(_, _, imm)
            | Inst::LOAD(_, _, imm)
            | Inst::LOADI(_, imm)
            | Inst::STORE(_, _, imm)
            | Inst::JUMP(imm)
            | Inst::JUMPR(imm)
            | Inst::JUMPIF(_, imm)
            | Inst::JUMPIFR(_, imm)
            | Inst::CALL(imm) => Some(imm),
            _ => None,
        }
    }
}

impl<R: Clone, I> Inst<R, I> {
    /// Resolves immediate values using the provided function
    pub fn resolve<O>(self, f: impl Fn(I) -> O) -> Inst<R, O> {
        match self {
            Inst::ADD(rd, rs1, rs2) => Inst::ADD(rd, rs1, rs2),
            Inst::ADDI(rd, rs, imm) => Inst::ADDI(rd, rs, f(imm)),
            Inst::SUB(rd, rs1, rs2) => Inst::SUB(rd, rs1, rs2),
            Inst::SUBI(rd, rs, imm) => Inst::SUBI(rd, rs, f(imm)),
            Inst::NOT(rd, rs) => Inst::NOT(rd, rs),
            Inst::AND(rd, rs1, rs2) => Inst::AND(rd, rs1, rs2),
            Inst::ANDI(rd, rs, imm) => Inst::ANDI(rd, rs, f(imm)),
            Inst::OR(rd, rs1, rs2) => Inst::OR(rd, rs1, rs2),
            Inst::ORI(rd, rs, imm) => Inst::ORI(rd, rs, f(imm)),
            Inst::XOR(rd, rs1, rs2) => Inst::XOR(rd, rs1, rs2),
            Inst::XORI(rd, rs, imm) => Inst::XORI(rd, rs, f(imm)),
            Inst::EQ(rd, rs1, rs2) => Inst::EQ(rd, rs1, rs2),
            Inst::EQI(rd, rs, imm) => Inst::EQI(rd, rs, f(imm)),
            Inst::NEQ(rd, rs1, rs2) => Inst::NEQ(rd, rs1, rs2),
            Inst::NEQI(rd, rs, imm) => Inst::NEQI(rd, rs, f(imm)),
            Inst::LT(rd, rs1, rs2) => Inst::LT(rd, rs1, rs2),
            Inst::LTI(rd, rs, imm) => Inst::LTI(rd, rs, f(imm)),
            Inst::LTS(rd, rs1, rs2) => Inst::LTS(rd, rs1, rs2),
            Inst::LTSI(rd, rs, imm) => Inst::LTSI(rd, rs, f(imm)),
            Inst::SR(rd, rs) => Inst::SR(rd, rs),
            Inst::SRS(rd, rs) => Inst::SRS(rd, rs),
            Inst::SRR(rd, rs) => Inst::SRR(rd, rs),
            Inst::SL(rd, rs) => Inst::SL(rd, rs),
            Inst::SLR(rd, rs) => Inst::SLR(rd, rs),
            Inst::MOV(rd, rs) => Inst::MOV(rd, rs),
            Inst::LOAD(rd, rs, imm) => Inst::LOAD(rd, rs, f(imm)),
            Inst::LOADI(rd, imm) => Inst::LOADI(rd, f(imm)),
            Inst::STORE(rd, rs, imm) => Inst::STORE(rd, rs, f(imm)),
            Inst::NOP() => Inst::NOP(),
            Inst::JUMP(imm) => Inst::JUMP(f(imm)),
            Inst::JUMPR(imm) => Inst::JUMPR(f(imm)),
            Inst::JUMPIF(rd, imm) => Inst::JUMPIF(rd, f(imm)),
            Inst::JUMPIFR(rd, imm) => Inst::JUMPIFR(rd, f(imm)),
            Inst::CALL(imm) => Inst::CALL(f(imm)),
            Inst::RET() => Inst::RET(),
            Inst::IRET() => Inst::IRET(),
        }
    }
}

impl Inst<Reg, u16> {
    pub fn to_op(self) -> Op {
        match self {
            Inst::ADD(rd, rs1, rs2) => Op::CALC(ALU::ADD, rd, rs1, rs2),
            Inst::ADDI(rd, rs, imm) => Op::CALCI(ALU::ADD, rd, rs, imm),
            Inst::SUB(rd, rs1, rs2) => Op::CALC(ALU::SUB, rd, rs1, rs2),
            Inst::SUBI(rd, rs, imm) => Op::CALCI(ALU::SUB, rd, rs, imm),

            Inst::NOT(rd, rs) => Op::CALCI(ALU::XOR, rd, rs, 0xFFFF),
            Inst::AND(rd, rs1, rs2) => Op::CALC(ALU::AND, rd, rs1, rs2),
            Inst::ANDI(rd, rs, imm) => Op::CALCI(ALU::AND, rd, rs, imm),
            Inst::OR(rd, rs1, rs2) => Op::CALC(ALU::OR, rd, rs1, rs2),
            Inst::ORI(rd, rs, imm) => Op::CALCI(ALU::OR, rd, rs, imm),
            Inst::XOR(rd, rs1, rs2) => Op::CALC(ALU::XOR, rd, rs1, rs2),
            Inst::XORI(rd, rs, imm) => Op::CALCI(ALU::XOR, rd, rs, imm),

            Inst::EQ(rd, rs1, rs2) => Op::CALC(ALU::EQ, rd, rs1, rs2),
            Inst::EQI(rd, rs, imm) => Op::CALCI(ALU::EQ, rd, rs, imm),
            Inst::NEQ(rd, rs1, rs2) => Op::CALC(ALU::NEQ, rd, rs1, rs2),
            Inst::NEQI(rd, rs, imm) => Op::CALCI(ALU::NEQ, rd, rs, imm),
            Inst::LT(rd, rs1, rs2) => Op::CALC(ALU::LT, rd, rs1, rs2),
            Inst::LTI(rd, rs, imm) => Op::CALCI(ALU::LT, rd, rs, imm),
            Inst::LTS(rd, rs1, rs2) => Op::CALC(ALU::LTS, rd, rs1, rs2),
            Inst::LTSI(rd, rs, imm) => Op::CALCI(ALU::LTS, rd, rs, imm),

            Inst::SR(rd, rs) => Op::CALC(ALU::SR, rd, rs, Reg::Z),
            Inst::SRS(rd, rs) => Op::CALC(ALU::SRS, rd, rs, Reg::Z),
            Inst::SRR(rd, rs) => Op::CALC(ALU::SRR, rd, rs, Reg::Z),
            Inst::SL(rd, rs) => Op::CALC(ALU::SL, rd, rs, Reg::Z),
            Inst::SLR(rd, rs) => Op::CALC(ALU::SLR, rd, rs, Reg::Z),

            Inst::MOV(rd, rs) => Op::CALC(ALU::ADD, rd, rs, Reg::Z),
            Inst::LOAD(rd, rs, imm) => Op::LOAD(rd, rs, imm),
            Inst::LOADI(rd, imm) => Op::CALCI(ALU::ADD, rd, Reg::Z, imm),
            Inst::STORE(rd, rs, imm) => Op::STORE(rd, rs, imm),

            Inst::NOP() => Op::CALC(ALU::ADD, Reg::Z, Reg::Z, Reg::Z),
            Inst::JUMP(imm) => Op::CTRL(Reg::Z, Reg::Z, Reg::Z, imm),
            Inst::JUMPR(imm) => Op::CTRL(Reg::Z, Reg::PC, Reg::Z, imm),
            Inst::JUMPIF(rs, imm) => Op::CTRL(Reg::Z, Reg::Z, rs, imm),
            Inst::JUMPIFR(rs, imm) => Op::CTRL(Reg::Z, Reg::PC, rs, imm),
            Inst::CALL(imm) => Op::CTRL(Reg::RA, Reg::Z, Reg::Z, imm),
            Inst::RET() => Op::CTRL(Reg::Z, Reg::RA, Reg::Z, 0),
            Inst::IRET() => Op::CTRL(Reg::Z, Reg::IRA, Reg::Z, 0),
        }
    }

    pub fn from_op(op: Op) -> Inst<Reg, u16> {
        match op {
            Op::CALC(alu, rd, rs1, rs2) => match alu {
                ALU::ADD => match (rs1, rs2) {
                    (Reg::Z, Reg::Z) => Inst::NOP(),
                    (_, Reg::Z) => Inst::MOV(rd, rs1),
                    (_, _) => Inst::ADD(rd, rs1, rs2),
                },
                ALU::SUB => Inst::SUB(rd, rs1, rs2),
                ALU::AND => Inst::AND(rd, rs1, rs2),
                ALU::OR => Inst::OR(rd, rs1, rs2),
                ALU::XOR => Inst::XOR(rd, rs1, rs2),
                ALU::EQ => Inst::EQ(rd, rs1, rs2),
                ALU::NEQ => Inst::NEQ(rd, rs1, rs2),
                ALU::LT => Inst::LT(rd, rs1, rs2),
                ALU::LTS => Inst::LTS(rd, rs1, rs2),
                ALU::SR => Inst::SR(rd, rs1),
                ALU::SRS => Inst::SRS(rd, rs1),
                ALU::SRR => Inst::SRR(rd, rs1),
                ALU::SL => Inst::SL(rd, rs1),
                ALU::SLR => Inst::SLR(rd, rs1),
            },
            Op::CALCI(alu, rd, rs1, imm) => match alu {
                ALU::ADD => match rs1 {
                    Reg::Z => Inst::LOADI(rd, imm),
                    _ => Inst::ADDI(rd, rs1, imm),
                },
                ALU::SUB => Inst::SUBI(rd, rs1, imm),
                ALU::AND => Inst::ANDI(rd, rs1, imm),
                ALU::OR => Inst::ORI(rd, rs1, imm),
                ALU::XOR => match imm {
                    0xFFFF => Inst::NOT(rd, rs1),
                    _ => Inst::XORI(rd, rs1, imm),
                },
                ALU::EQ => Inst::EQI(rd, rs1, imm),
                ALU::NEQ => Inst::NEQI(rd, rs1, imm),
                ALU::LT => Inst::LTI(rd, rs1, imm),
                ALU::LTS => Inst::LTSI(rd, rs1, imm),
                ALU::SR => Inst::SR(rd, rs1),
                ALU::SRS => Inst::SRS(rd, rs1),
                ALU::SRR => Inst::SRR(rd, rs1),
                ALU::SL => Inst::SL(rd, rs1),
                ALU::SLR => Inst::SLR(rd, rs1),
            },
            Op::LOAD(rd, rs, imm) => Inst::LOAD(rd, rs, imm),
            Op::STORE(rs2, rs1, imm) => Inst::STORE(rs2, rs1, imm),
            Op::CTRL(rd, rs1, rs2, imm) => match (rd, rs1, rs2) {
                (Reg::Z, Reg::Z, Reg::Z) => Inst::JUMP(imm),
                (Reg::Z, Reg::PC, Reg::Z) => Inst::JUMPR(imm),
                (Reg::Z, Reg::Z, rs2) => Inst::JUMPIF(rs2, imm),
                (Reg::Z, Reg::PC, rs2) => Inst::JUMPIFR(rs2, imm),
                (Reg::RA, _, _) => Inst::CALL(imm),
                (Reg::Z, Reg::RA, Reg::Z) => Inst::RET(),
                (Reg::Z, Reg::IRA, Reg::Z) => Inst::IRET(),
                _ => panic!("Undefined Inst: {:?}", op),
            },
        }
    }
}

impl<R: Display, I: Display + std::fmt::LowerHex + std::fmt::UpperHex> Inst<R, I> {
    pub fn cformat(&self) -> String {
        macro_rules! rrr {
            ($name:expr, $rd:expr, $rs1:expr, $rs2:expr) => {
                cformat!("<r>{:<6}</><b>{:<2} {:<2} {:<2}</>", $name, $rd, $rs1, $rs2)
            };
        }

        macro_rules! rri {
            ($name:expr, $rd:expr, $rs1:expr, $imm:expr) => {
                cformat!(
                    "<r>{:<6}</><b>{:<2} {:<2} <y>0x{:0>4X}</></>",
                    $name,
                    $rd,
                    $rs1,
                    $imm
                )
            };
        }

        match self {
            Inst::ADD(rd, rs1, rs2) => rrr!("add", rd, rs1, rs2),
            Inst::SUB(rd, rs1, rs2) => rrr!("sub", rd, rs1, rs2),
            Inst::AND(rd, rs1, rs2) => rrr!("and", rd, rs1, rs2),
            Inst::OR(rd, rs1, rs2) => rrr!("or", rd, rs1, rs2),
            Inst::XOR(rd, rs1, rs2) => rrr!("xor", rd, rs1, rs2),
            Inst::EQ(rd, rs1, rs2) => rrr!("eq", rd, rs1, rs2),
            Inst::NEQ(rd, rs1, rs2) => rrr!("neq", rd, rs1, rs2),
            Inst::LT(rd, rs1, rs2) => rrr!("lt", rd, rs1, rs2),
            Inst::LTS(rd, rs1, rs2) => rrr!("lts", rd, rs1, rs2),
            Inst::SR(rd, rs1) => rrr!("sr", rd, rs1, ""),
            Inst::SRS(rd, rs1) => rrr!("srs", rd, rs1, ""),
            Inst::SRR(rd, rs1) => rrr!("srr", rd, rs1, ""),
            Inst::SL(rd, rs1) => rrr!("sl", rd, rs1, ""),
            Inst::SLR(rd, rs1) => rrr!("slr", rd, rs1, ""),
            Inst::NOP() => rrr!("nop", "", "", ""),
            Inst::MOV(rd, rs1) => rrr!("mov", rd, rs1, ""),
            Inst::ADDI(rd, rs1, imm) => rri!("addi", rd, rs1, imm),
            Inst::SUBI(rd, rs1, imm) => rri!("subi", rd, rs1, imm),
            Inst::ANDI(rd, rs1, imm) => rri!("andi", rd, rs1, imm),
            Inst::ORI(rd, rs1, imm) => rri!("ori", rd, rs1, imm),
            Inst::XORI(rd, rs1, imm) => rri!("xori", rd, rs1, imm),
            Inst::EQI(rd, rs1, imm) => rri!("eqi", rd, rs1, imm),
            Inst::NEQI(rd, rs1, imm) => rri!("neqi", rd, rs1, imm),
            Inst::LTI(rd, rs1, imm) => rri!("lti", rd, rs1, imm),
            Inst::LTSI(rd, rs1, imm) => rri!("ltsi", rd, rs1, imm),
            Inst::NOT(rd, rs1) => rrr!("not", rd, rs1, ""),
            Inst::LOADI(rd, imm) => rri!("loadi", rd, "", imm),
            Inst::LOAD(rd, rs1, imm) => rri!("load", rd, rs1, imm),
            Inst::STORE(rs2, rs1, imm) => rri!("store", rs2, rs1, imm),
            Inst::JUMPIF(rs2, imm) => rri!("jumpif", rs2, "", imm),
            Inst::JUMPIFR(rs2, imm) => rri!("jumpifr", rs2, "", imm),
            Inst::JUMP(imm) => rri!("jump", "", "", imm),
            Inst::JUMPR(imm) => rri!("jumpr", "", "", imm),
            Inst::CALL(imm) => rri!("call", "", "", imm),
            Inst::RET() => rrr!("ret", "", "", ""),
            Inst::IRET() => rrr!("iret", "", "", ""),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_inst {
        ($($name:ident: $inst:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let inst: Inst<Reg, u16> = $inst;
                    let op = inst.clone().to_op();
                    let inst_back = Inst::<Reg, u16>::from_op(op);
                    assert_eq!(inst, inst_back);
                }
            )*
        }
    }

    test_inst! {
        test_add: Inst::ADD(Reg::T0, Reg::T1, Reg::T2),
        test_addi: Inst::ADDI(Reg::T0, Reg::T1, 0x0123),
        test_sub: Inst::SUB(Reg::T0, Reg::T1, Reg::T2),
        test_subi: Inst::SUBI(Reg::T0, Reg::T1, 0x0123),
        test_not: Inst::NOT(Reg::T0, Reg::T1),
        test_and: Inst::AND(Reg::T0, Reg::T1, Reg::T2),
        test_andi: Inst::ANDI(Reg::T0, Reg::T1, 0x0123),
        test_or: Inst::OR(Reg::T0, Reg::T1, Reg::T2),
        test_ori: Inst::ORI(Reg::T0, Reg::T1, 0x0123),
        test_xor: Inst::XOR(Reg::T0, Reg::T1, Reg::T2),
        test_xori: Inst::XORI(Reg::T0, Reg::T1, 0x0123),
        test_eq: Inst::EQ(Reg::T0, Reg::T1, Reg::T2),
        test_eqi: Inst::EQI(Reg::T0, Reg::T1, 0x0123),
        test_neq: Inst::NEQ(Reg::T0, Reg::T1, Reg::T2),
        test_neqi: Inst::NEQI(Reg::T0, Reg::T1, 0x0123),
        test_lt: Inst::LT(Reg::T0, Reg::T1, Reg::T2),
        test_lti: Inst::LTI(Reg::T0, Reg::T1, 0x0123),
        test_lts: Inst::LTS(Reg::T0, Reg::T1, Reg::T2),
        test_ltsi: Inst::LTSI(Reg::T0, Reg::T1, 0x0123),
        test_sr: Inst::SR(Reg::T0, Reg::T1),
        test_srs: Inst::SRS(Reg::T0, Reg::T1),
        test_srr: Inst::SRR(Reg::T0, Reg::T1),
        test_sl: Inst::SL(Reg::T0, Reg::T1),
        test_slr: Inst::SLR(Reg::T0, Reg::T1),
        test_mov: Inst::MOV(Reg::T0, Reg::T1),
        test_load: Inst::LOAD(Reg::T0, Reg::T1, 0x0123),
        test_loadi: Inst::LOADI(Reg::T0, 0x0123),
        test_store: Inst::STORE(Reg::T0, Reg::T1, 0x0123),
        test_nop: Inst::NOP(),
        test_jumpif: Inst::JUMPIF(Reg::T0, 0x0123),
        test_jumpifr: Inst::JUMPIFR(Reg::T0, 0x0123),
        test_jump: Inst::JUMP(0x0123),
        test_jumpr: Inst::JUMPR(0x0123),
        test_call: Inst::CALL(0x0123),
        test_ret: Inst::RET(),
        test_iret: Inst::IRET(),
    }
}
