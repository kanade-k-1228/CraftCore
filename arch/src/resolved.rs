// ----------------------------------------------------------------------------

pub struct Reg;

impl Reg {
    pub const ZERO: u8 = 0;
    pub const IRA: u8 = 1;
    pub const PC: u8 = 2;
    pub const SP: u8 = 3;
    pub const RA: u8 = 4;
    pub const FP: u8 = 5;
    pub const A0: u8 = 6;
    pub const A1: u8 = 7;
    pub const T0: u8 = 8;
    pub const T1: u8 = 9;
    pub const T2: u8 = 10;
    pub const T3: u8 = 11;
    pub const S0: u8 = 12;
    pub const S1: u8 = 13;
    pub const S2: u8 = 14;
    pub const S3: u8 = 15;
}

// ----------------------------------------------------------------------------

pub struct OpCode;

impl OpCode {
    pub const CALC: u8 = 0b0000;
    pub const CALCI: u8 = 0b0001;
    pub const LOAD: u8 = 0b0011;
    pub const STORE: u8 = 0b0111;
    pub const CTRL: u8 = 0b1111;
}

// ----------------------------------------------------------------------------

pub struct ALU;

impl ALU {
    pub const ADD: u8 = 0;
    pub const SUB: u8 = 1;
    pub const AND: u8 = 2;
    pub const OR: u8 = 3;
    pub const XOR: u8 = 4;
    pub const EQ: u8 = 5;
    pub const NEQ: u8 = 6;
    pub const LT: u8 = 7;
    pub const LTS: u8 = 8;
    pub const SR: u8 = 9;
    pub const SRS: u8 = 10;
    pub const SRR: u8 = 11;
    pub const SL: u8 = 12;
    pub const SLR: u8 = 13;
}

// ----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    ADD(u8, u8, u8),
    SUB(u8, u8, u8),
    AND(u8, u8, u8),
    OR(u8, u8, u8),
    XOR(u8, u8, u8),
    EQ(u8, u8, u8),
    NEQ(u8, u8, u8),
    LT(u8, u8, u8),
    LTS(u8, u8, u8),
    SR(u8, u8),
    SRS(u8, u8),
    SRR(u8, u8),
    SL(u8, u8),
    SLR(u8, u8),
    ADDI(u8, u8, u16),
    SUBI(u8, u8, u16),
    ANDI(u8, u8, u16),
    ORI(u8, u8, u16),
    XORI(u8, u8, u16),
    EQI(u8, u8, u16),
    NEQI(u8, u8, u16),
    LTI(u8, u8, u16),
    LTSI(u8, u8, u16),
    LOAD(u8, u8, u16),
    STORE(u8, u8, u16),
    CTRL(u8, u8, u8, u16),
}

// ----------------------------------------------------------------------------

fn enc_format(opcode: u8, rd: u8, rs1: u8, rs2: u8, imm: u16) -> u32 {
    ((opcode as u32) << 0)
        | ((rs1 as u32) << 4)
        | ((rs2 as u32) << 8)
        | ((rd as u32) << 12)
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
            Op::ADD(rd, rs1, rs2) => enc_format(OpCode::CALC, rd, rs1, rs2, ALU::ADD as u16),
            Op::SUB(rd, rs1, rs2) => enc_format(OpCode::CALC, rd, rs1, rs2, ALU::SUB as u16),
            Op::AND(rd, rs1, rs2) => enc_format(OpCode::CALC, rd, rs1, rs2, ALU::AND as u16),
            Op::OR(rd, rs1, rs2) => enc_format(OpCode::CALC, rd, rs1, rs2, ALU::OR as u16),
            Op::XOR(rd, rs1, rs2) => enc_format(OpCode::CALC, rd, rs1, rs2, ALU::XOR as u16),
            Op::EQ(rd, rs1, rs2) => enc_format(OpCode::CALC, rd, rs1, rs2, ALU::EQ as u16),
            Op::NEQ(rd, rs1, rs2) => enc_format(OpCode::CALC, rd, rs1, rs2, ALU::NEQ as u16),
            Op::LT(rd, rs1, rs2) => enc_format(OpCode::CALC, rd, rs1, rs2, ALU::LT as u16),
            Op::LTS(rd, rs1, rs2) => enc_format(OpCode::CALC, rd, rs1, rs2, ALU::LTS as u16),
            Op::SR(rd, rs1) => enc_format(OpCode::CALC, rd, rs1, 0, ALU::SR as u16),
            Op::SRS(rd, rs1) => enc_format(OpCode::CALC, rd, rs1, 0, ALU::SRS as u16),
            Op::SRR(rd, rs1) => enc_format(OpCode::CALC, rd, rs1, 0, ALU::SRR as u16),
            Op::SL(rd, rs1) => enc_format(OpCode::CALC, rd, rs1, 0, ALU::SL as u16),
            Op::SLR(rd, rs1) => enc_format(OpCode::CALC, rd, rs1, 0, ALU::SLR as u16),
            Op::ADDI(rd, rs1, imm) => enc_format(OpCode::CALCI, rd, rs1, ALU::ADD, imm),
            Op::SUBI(rd, rs1, imm) => enc_format(OpCode::CALCI, rd, rs1, ALU::SUB, imm),
            Op::ANDI(rd, rs1, imm) => enc_format(OpCode::CALCI, rd, rs1, ALU::AND, imm),
            Op::ORI(rd, rs1, imm) => enc_format(OpCode::CALCI, rd, rs1, ALU::OR, imm),
            Op::XORI(rd, rs1, imm) => enc_format(OpCode::CALCI, rd, rs1, ALU::XOR, imm),
            Op::EQI(rd, rs1, imm) => enc_format(OpCode::CALCI, rd, rs1, ALU::EQ, imm),
            Op::NEQI(rd, rs1, imm) => enc_format(OpCode::CALCI, rd, rs1, ALU::NEQ, imm),
            Op::LTI(rd, rs1, imm) => enc_format(OpCode::CALCI, rd, rs1, ALU::LT, imm),
            Op::LTSI(rd, rs1, imm) => enc_format(OpCode::CALCI, rd, rs1, ALU::LTS, imm),
            Op::LOAD(rd, rs1, imm) => enc_format(OpCode::LOAD, rd, rs1, 0, imm),
            Op::STORE(rs2, rs1, imm) => enc_format(OpCode::STORE, 0, rs1, rs2, imm),
            Op::CTRL(rd, rs1, rs2, imm) => enc_format(OpCode::CTRL, rd, rs1, rs2, imm),
        }
    }

    pub fn from_bin(bin: u32) -> Op {
        let (opcode, rd, rs1, rs2, imm) = dec_format(bin);
        match opcode {
            OpCode::CALC => match imm as u8 {
                ALU::ADD => Op::ADD(rd, rs1, rs2),
                ALU::SUB => Op::SUB(rd, rs1, rs2),
                ALU::AND => Op::AND(rd, rs1, rs2),
                ALU::OR => Op::OR(rd, rs1, rs2),
                ALU::XOR => Op::XOR(rd, rs1, rs2),
                ALU::EQ => Op::EQ(rd, rs1, rs2),
                ALU::NEQ => Op::NEQ(rd, rs1, rs2),
                ALU::LT => Op::LT(rd, rs1, rs2),
                ALU::LTS => Op::LTS(rd, rs1, rs2),
                ALU::SR => Op::SR(rd, rs1),
                ALU::SRS => Op::SRS(rd, rs1),
                ALU::SRR => Op::SRR(rd, rs1),
                ALU::SL => Op::SL(rd, rs1),
                ALU::SLR => Op::SLR(rd, rs1),
                _ => panic!("Unknown ALU operation"),
            },
            OpCode::CALCI => match rs2 {
                ALU::ADD => Op::ADDI(rd, rs1, imm),
                ALU::SUB => Op::SUBI(rd, rs1, imm),
                ALU::AND => Op::ANDI(rd, rs1, imm),
                ALU::OR => Op::ORI(rd, rs1, imm),
                ALU::XOR => Op::XORI(rd, rs1, imm),
                ALU::EQ => Op::EQI(rd, rs1, imm),
                ALU::NEQ => Op::NEQI(rd, rs1, imm),
                ALU::LT => Op::LTI(rd, rs1, imm),
                ALU::LTS => Op::LTSI(rd, rs1, imm),
                _ => panic!("Unknown ALU operation"),
            },
            OpCode::LOAD => Op::LOAD(rd, rs1, imm),
            OpCode::STORE => Op::STORE(rs2, rs1, imm),
            OpCode::CTRL => Op::CTRL(rd, rs1, rs2, imm),
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

test_op!(test_add, Op::ADD(1, 2, 3));
test_op!(test_sub, Op::SUB(1, 2, 3));
test_op!(test_and, Op::AND(1, 2, 3));
test_op!(test_or, Op::OR(1, 2, 3));
test_op!(test_xor, Op::XOR(1, 2, 3));
test_op!(test_eq, Op::EQ(1, 2, 3));
test_op!(test_neq, Op::NEQ(1, 2, 3));
test_op!(test_lt, Op::LT(1, 2, 3));
test_op!(test_lts, Op::LTS(1, 2, 3));
test_op!(test_sr, Op::SR(1, 2));
test_op!(test_srs, Op::SRS(1, 2));
test_op!(test_srr, Op::SRR(1, 2));
test_op!(test_sl, Op::SL(1, 2));
test_op!(test_slr, Op::SLR(1, 2));
test_op!(test_addi, Op::ADDI(1, 2, 42));
test_op!(test_subi, Op::SUBI(1, 2, 42));
test_op!(test_andi, Op::ANDI(1, 2, 42));
test_op!(test_ori, Op::ORI(1, 2, 42));
test_op!(test_xori, Op::XORI(1, 2, 42));
test_op!(test_eqi, Op::EQI(1, 2, 42));
test_op!(test_neqi, Op::NEQI(1, 2, 42));
test_op!(test_lti, Op::LTI(1, 2, 42));
test_op!(test_ltsi, Op::LTSI(1, 2, 42));
test_op!(test_load, Op::LOAD(1, 2, 42));
test_op!(test_store, Op::STORE(1, 2, 42));
test_op!(test_ctrl, Op::CTRL(1, 2, 3, 42));
