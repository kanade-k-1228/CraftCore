use crate::resolved::{Op, Reg};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Asm {
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

    // Psudo Ops
    NOP,         // add 0 0 0
    MOV(u8, u8), // add rd rs1 0
    NOT(u8, u8),
    LOADI(u8, u16),
    IF(u8, u16),
    IFR(u8, u16),
    JUMP(u16),
    JUMPR(u16),
    CALL(u16),
    RET,
    IRET,
}

impl Asm {
    pub fn to_op(self) -> Op {
        match self {
            Asm::ADD(rs1, rs2, rd) => Op::ADD(rd, rs1, rs2),
            Asm::SUB(rs1, rs2, rd) => Op::SUB(rd, rs1, rs2),
            Asm::AND(rs1, rs2, rd) => Op::AND(rd, rs1, rs2),
            Asm::OR(rs1, rs2, rd) => Op::OR(rd, rs1, rs2),
            Asm::XOR(rs1, rs2, rd) => Op::XOR(rd, rs1, rs2),
            Asm::EQ(rs1, rs2, rd) => Op::EQ(rd, rs1, rs2),
            Asm::NEQ(rs1, rs2, rd) => Op::NEQ(rd, rs1, rs2),
            Asm::LT(rs1, rs2, rd) => Op::LT(rd, rs1, rs2),
            Asm::LTS(rs1, rs2, rd) => Op::LTS(rd, rs1, rs2),
            Asm::SR(rs, rd) => Op::SR(rd, rs),
            Asm::SRS(rs, rd) => Op::SRS(rd, rs),
            Asm::SRR(rs, rd) => Op::SRR(rd, rs),
            Asm::SL(rs, rd) => Op::SL(rd, rs),
            Asm::SLR(rs, rd) => Op::SLR(rd, rs),
            Asm::ADDI(rs, rd, imm) => Op::ADDI(rd, rs, imm),
            Asm::SUBI(rs, rd, imm) => Op::SUBI(rd, rs, imm),
            Asm::ANDI(rs, rd, imm) => Op::ANDI(rd, rs, imm),
            Asm::ORI(rs, rd, imm) => Op::ORI(rd, rs, imm),
            Asm::XORI(rs, rd, imm) => Op::XORI(rd, rs, imm),
            Asm::EQI(rs, rd, imm) => Op::EQI(rd, rs, imm),
            Asm::NEQI(rs, rd, imm) => Op::NEQI(rd, rs, imm),
            Asm::LTI(rs, rd, imm) => Op::LTI(rd, rs, imm),
            Asm::LTSI(rs, rd, imm) => Op::LTSI(rd, rs, imm),
            Asm::LOAD(rs, rd, imm) => Op::LOAD(rd, rs, imm),
            Asm::STORE(rs, rd, imm) => Op::STORE(rd, rs, imm),
            Asm::CTRL(rs1, rs2, rd, imm) => Op::CTRL(rd, rs1, rs2, imm),

            // Psudo Op Mapping
            Asm::NOP => Op::ADD(0, 0, 0),
            Asm::MOV(rs, rd) => Op::ADD(rd, rs, 0),
            Asm::NOT(rs, rd) => Op::XOR(rd, rs, 0xFF),
            Asm::LOADI(rd, imm) => Op::LOAD(rd, 0, imm),
            Asm::IF(rs, imm) => Op::CTRL(0, rs, 0, imm),
            Asm::IFR(rs, imm) => Op::CTRL(0, rs, 0, imm),
            Asm::JUMP(imm) => Op::CTRL(0, 0, 0, imm),
            Asm::JUMPR(imm) => Op::CTRL(0, 0, 0, imm),
            Asm::CALL(imm) => Op::CTRL(0, 0, 0, imm),
            Asm::RET => Op::CTRL(0, Reg::RA, 0, 0),
            Asm::IRET => Op::CTRL(0, Reg::IRA, 0, 0),
        }
    }

    pub fn from_op(op: Op) -> Asm {
        match op {
            Op::ADD(rd, rs1, rs2) => {
                if rs2 == 0 {
                    return Asm::MOV(rs1, rd);
                }
                Asm::ADD(rs1, rs2, rd)
            }
            Op::SUB(rd, rs1, rs2) => Asm::SUB(rs1, rs2, rd),
            Op::AND(rd, rs1, rs2) => Asm::AND(rs1, rs2, rd),
            Op::OR(rd, rs1, rs2) => Asm::OR(rs1, rs2, rd),
            Op::XOR(rd, rs1, rs2) => Asm::XOR(rs1, rs2, rd),
            Op::EQ(rd, rs1, rs2) => Asm::EQ(rs1, rs2, rd),
            Op::NEQ(rd, rs1, rs2) => Asm::NEQ(rs1, rs2, rd),
            Op::LT(rd, rs1, rs2) => Asm::LT(rs1, rs2, rd),
            Op::LTS(rd, rs1, rs2) => Asm::LTS(rs1, rs2, rd),
            Op::SR(rd, rs) => Asm::SR(rs, rd),
            Op::SRS(rd, rs) => Asm::SRS(rs, rd),
            Op::SRR(rd, rs) => Asm::SRR(rs, rd),
            Op::SL(rd, rs) => Asm::SL(rs, rd),
            Op::SLR(rd, rs) => Asm::SLR(rs, rd),
            Op::ADDI(rd, rs, imm) => Asm::ADDI(rs, rd, imm),
            Op::SUBI(rd, rs, imm) => Asm::SUBI(rs, rd, imm),
            Op::ANDI(rd, rs, imm) => Asm::ANDI(rs, rd, imm),
            Op::ORI(rd, rs, imm) => Asm::ORI(rs, rd, imm),
            Op::XORI(rd, rs, imm) => Asm::XORI(rs, rd, imm),
            Op::EQI(rd, rs, imm) => Asm::EQI(rs, rd, imm),
            Op::NEQI(rd, rs, imm) => Asm::NEQI(rs, rd, imm),
            Op::LTI(rd, rs, imm) => Asm::LTI(rs, rd, imm),
            Op::LTSI(rd, rs, imm) => Asm::LTSI(rs, rd, imm),
            Op::LOAD(rd, rs, imm) => Asm::LOAD(rs, rd, imm),
            Op::STORE(rd, rs, imm) => Asm::STORE(rs, rd, imm),
            Op::CTRL(rd, rs1, rs2, imm) => {
                if rs2 != 0 {
                    return Asm::IF(rs2, imm);
                }
                if rs1 == Reg::IRA && rs2 == 0 && rd == 0 {
                    return Asm::IRET;
                }
                if rs1 == Reg::RA && rs2 == 0 && rd == 0 {
                    return Asm::RET;
                }
                if rd == Reg::RA {
                    return Asm::CALL(imm);
                }
                if rs1 == 0 && rs2 == 0 {
                    return Asm::JUMP(imm);
                }
                if rd == 0 {
                    return Asm::IF(rs2, imm);
                }
                Asm::CTRL(rd, rs1, rs2, imm)
            }
        }
    }
}
