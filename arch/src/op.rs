// // ----------------------------------------------------------------------------

// pub struct Reg;

// impl Reg {
//     pub const ZERO: u8 = 0;
//     pub const IRA: u8 = 1;
//     pub const PC: u8 = 2;
//     pub const SP: u8 = 3;
//     pub const RA: u8 = 4;
//     pub const FP: u8 = 5;
//     pub const A0: u8 = 6;
//     pub const A1: u8 = 7;
//     pub const T0: u8 = 8;
//     pub const T1: u8 = 9;
//     pub const T2: u8 = 10;
//     pub const T3: u8 = 11;
//     pub const S0: u8 = 12;
//     pub const S1: u8 = 13;
//     pub const S2: u8 = 14;
//     pub const S3: u8 = 15;
// }

// // ----------------------------------------------------------------------------

// // ----------------------------------------------------------------------------

// pub struct ALU;

// impl ALU {
//     pub const ADD: u8 = 0;
//     pub const SUB: u8 = 1;
//     pub const AND: u8 = 2;
//     pub const OR: u8 = 3;
//     pub const XOR: u8 = 4;
//     pub const EQ: u8 = 5;
//     pub const NEQ: u8 = 6;
//     pub const LT: u8 = 7;
//     pub const LTS: u8 = 8;
//     pub const SR: u8 = 9;
//     pub const SRS: u8 = 10;
//     pub const SRR: u8 = 11;
//     pub const SL: u8 = 12;
//     pub const SLR: u8 = 13;
// }

// ----------------------------------------------------------------------------

use crate::{alu::ALU, reg::Reg};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    CALC(ALU, Reg, Reg, Reg),
    CALCI(ALU, Reg, Reg, u16),
    LOAD(Reg, Reg, u16),
    STORE(Reg, Reg, u16),
    CTRL(Reg, Reg, Reg, u16),
}

struct OpCode;

impl OpCode {
    pub const CALC: u8 = 0b0000;
    pub const CALCI: u8 = 0b0001;
    pub const LOAD: u8 = 0b0011;
    pub const STORE: u8 = 0b0111;
    pub const CTRL: u8 = 0b1111;
}

// ----------------------------------------------------------------------------

fn enc_format(
    opcode: u8,
    rd: impl Into<u8>,
    rs1: impl Into<u8>,
    rs2: impl Into<u8>,
    imm: u16,
) -> u32 {
    ((opcode as u32) << 0)
        | ((rs1.into() as u32) << 4)
        | ((rs2.into() as u32) << 8)
        | ((rd.into() as u32) << 12)
        | ((imm as u32) << 16)
}

fn dec_format(bin: u32) -> (u8, u8, u8, u8, u16) {
    let opcode = ((bin >> 0) & 0xF) as u8;
    let rs1 = ((bin >> 4) & 0xF) as u8;
    let rs2 = ((bin >> 8) & 0xF) as u8;
    let rd = ((bin >> 12) & 0xF) as u8;
    let imm = ((bin >> 16) & 0xFFFF) as u16;
    (opcode, rd, rs1, rs2, imm)
}

#[test]
fn test_format_all() {
    for opcode in 0..=0xF {
        for rd in 0..=0xF {
            for rs1 in 0..=0xF {
                for rs2 in 0..=0xF {
                    for imm in 0..=0xFFFF {
                        let bin = enc_format(opcode, rd, rs1, rs2, imm);
                        let (dec_opcode, dec_rd, dec_rs1, dec_rs2, dec_imm) = dec_format(bin);
                        assert_eq!(opcode, dec_opcode);
                        assert_eq!(rd, dec_rd);
                        assert_eq!(rs1, dec_rs1);
                        assert_eq!(rs2, dec_rs2);
                        assert_eq!(imm, dec_imm);
                    }
                }
            }
        }
    }
}

// ----------------------------------------------------------------------------

impl Op {
    pub fn to_bin(&self) -> u32 {
        match *self {
            Op::CALC(alu, rd, rs1, rs2) => enc_format(OpCode::CALC, rd, rs1, rs2, alu as u8 as u16),
            Op::CALCI(alu, rd, rs1, imm) => enc_format(OpCode::CALCI, rd, rs1, alu, imm),
            Op::LOAD(rd, rs1, imm) => enc_format(OpCode::LOAD, rd, rs1, 0, imm),
            Op::STORE(rs2, rs1, imm) => enc_format(OpCode::STORE, 0, rs1, rs2, imm),
            Op::CTRL(rd, rs1, rs2, imm) => enc_format(OpCode::CTRL, rd, rs1, rs2, imm),
        }
    }

    pub fn from_bin(bin: u32) -> Op {
        let (opcode, rd, rs1, rs2, imm) = dec_format(bin);
        match opcode {
            OpCode::CALC => Op::CALC(
                ALU::from(imm as u8),
                Reg::from(rd),
                Reg::from(rs1),
                Reg::from(rs2),
            ),
            OpCode::CALCI => Op::CALCI(ALU::from(rs2), Reg::from(rd), Reg::from(rs1), imm),
            OpCode::LOAD => Op::LOAD(Reg::from(rd), Reg::from(rs1), imm),
            OpCode::STORE => Op::STORE(Reg::from(rs2), Reg::from(rs1), imm),
            OpCode::CTRL => Op::CTRL(Reg::from(rd), Reg::from(rs1), Reg::from(rs2), imm),
            _ => panic!("Unknown opcode"),
        }
    }
}

// ----------------------------------------------------------------------------

macro_rules! test_op {
    ($name:ident, $op:expr) => {
        #[test]
        fn $name() {
            let op = $op;
            let bin = op.to_bin();
            let decoded_op = Op::from_bin(bin);
            println!("{:?} -> {:0>8X} -> {:?}", op, bin, decoded_op);
            assert_eq!(
                op, decoded_op,
                "op: {:?}, bin: {:b}, decoded_op: {:?}",
                op, bin, decoded_op
            );
        }
    };
}

test_op!(test_add, Op::CALC(ALU::ADD, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_sub, Op::CALC(ALU::SUB, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_and, Op::CALC(ALU::AND, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_or, Op::CALC(ALU::OR, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_xor, Op::CALC(ALU::XOR, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_eq, Op::CALC(ALU::EQ, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_neq, Op::CALC(ALU::NEQ, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_lt, Op::CALC(ALU::LT, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_lts, Op::CALC(ALU::LTS, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_sr, Op::CALC(ALU::SR, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_srs, Op::CALC(ALU::SRS, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_srr, Op::CALC(ALU::SRR, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_sl, Op::CALC(ALU::SL, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_slr, Op::CALC(ALU::SLR, Reg::ZERO, Reg::T1, Reg::T2));
test_op!(test_load, Op::LOAD(Reg::ZERO, Reg::T1, 42));
test_op!(test_store, Op::STORE(Reg::ZERO, Reg::T1, 42));
test_op!(test_ctrl, Op::CTRL(Reg::ZERO, Reg::T1, Reg::T2, 42));
