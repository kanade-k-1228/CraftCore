use crate::resolved::{Op, Reg};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Psudo {
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

impl Psudo {
    pub fn to_op(self) -> Op {
        match self {
            Psudo::ADD(rs1, rs2, rd) => Op::ADD(rd, rs1, rs2),
            Psudo::SUB(rs1, rs2, rd) => Op::SUB(rd, rs1, rs2),
            Psudo::AND(rs1, rs2, rd) => Op::AND(rd, rs1, rs2),
            Psudo::OR(rs1, rs2, rd) => Op::OR(rd, rs1, rs2),
            Psudo::XOR(rs1, rs2, rd) => Op::XOR(rd, rs1, rs2),
            Psudo::EQ(rs1, rs2, rd) => Op::EQ(rd, rs1, rs2),
            Psudo::NEQ(rs1, rs2, rd) => Op::NEQ(rd, rs1, rs2),
            Psudo::LT(rs1, rs2, rd) => Op::LT(rd, rs1, rs2),
            Psudo::LTS(rs1, rs2, rd) => Op::LTS(rd, rs1, rs2),
            Psudo::SR(rs, rd) => Op::SR(rd, rs),
            Psudo::SRS(rs, rd) => Op::SRS(rd, rs),
            Psudo::SRR(rs, rd) => Op::SRR(rd, rs),
            Psudo::SL(rs, rd) => Op::SL(rd, rs),
            Psudo::SLR(rs, rd) => Op::SLR(rd, rs),
            Psudo::ADDI(rs, rd, imm) => Op::ADDI(rd, rs, imm),
            Psudo::SUBI(rs, rd, imm) => Op::SUBI(rd, rs, imm),
            Psudo::ANDI(rs, rd, imm) => Op::ANDI(rd, rs, imm),
            Psudo::ORI(rs, rd, imm) => Op::ORI(rd, rs, imm),
            Psudo::XORI(rs, rd, imm) => Op::XORI(rd, rs, imm),
            Psudo::EQI(rs, rd, imm) => Op::EQI(rd, rs, imm),
            Psudo::NEQI(rs, rd, imm) => Op::NEQI(rd, rs, imm),
            Psudo::LTI(rs, rd, imm) => Op::LTI(rd, rs, imm),
            Psudo::LTSI(rs, rd, imm) => Op::LTSI(rd, rs, imm),
            Psudo::LOAD(rs, rd, imm) => Op::LOAD(rd, rs, imm),
            Psudo::STORE(rs, rd, imm) => Op::STORE(rd, rs, imm),
            Psudo::CTRL(rs1, rs2, rd, imm) => Op::CTRL(rd, rs1, rs2, imm),

            // Psudo Op Mapping
            Psudo::NOP => Op::ADD(0, 0, 0),
            Psudo::MOV(rs, rd) => Op::ADD(rd, rs, 0),
            Psudo::NOT(rs, rd) => Op::XOR(rd, rs, 0xFF),
            Psudo::LOADI(rd, imm) => Op::LOAD(rd, 0, imm),
            Psudo::IF(rs, imm) => Op::CTRL(0, rs, 0, imm),
            Psudo::IFR(rs, imm) => Op::CTRL(0, rs, 0, imm),
            Psudo::JUMP(imm) => Op::CTRL(0, 0, 0, imm),
            Psudo::JUMPR(imm) => Op::CTRL(0, 0, 0, imm),
            Psudo::CALL(imm) => Op::CTRL(0, 0, 0, imm),
            Psudo::RET => Op::CTRL(0, Reg::RA, 0, 0),
            Psudo::IRET => Op::CTRL(0, Reg::IRA, 0, 0),
        }
    }

    pub fn from_op(op: Op) -> Psudo {
        match op {
            Op::ADD(rd, rs1, rs2) => {
                if rs2 == 0 {
                    return Psudo::MOV(rs1, rd);
                }
                Psudo::ADD(rs1, rs2, rd)
            }
            Op::SUB(rd, rs1, rs2) => Psudo::SUB(rs1, rs2, rd),
            Op::AND(rd, rs1, rs2) => Psudo::AND(rs1, rs2, rd),
            Op::OR(rd, rs1, rs2) => Psudo::OR(rs1, rs2, rd),
            Op::XOR(rd, rs1, rs2) => Psudo::XOR(rs1, rs2, rd),
            Op::EQ(rd, rs1, rs2) => Psudo::EQ(rs1, rs2, rd),
            Op::NEQ(rd, rs1, rs2) => Psudo::NEQ(rs1, rs2, rd),
            Op::LT(rd, rs1, rs2) => Psudo::LT(rs1, rs2, rd),
            Op::LTS(rd, rs1, rs2) => Psudo::LTS(rs1, rs2, rd),
            Op::SR(rd, rs) => Psudo::SR(rs, rd),
            Op::SRS(rd, rs) => Psudo::SRS(rs, rd),
            Op::SRR(rd, rs) => Psudo::SRR(rs, rd),
            Op::SL(rd, rs) => Psudo::SL(rs, rd),
            Op::SLR(rd, rs) => Psudo::SLR(rs, rd),
            Op::ADDI(rd, rs, imm) => Psudo::ADDI(rs, rd, imm),
            Op::SUBI(rd, rs, imm) => Psudo::SUBI(rs, rd, imm),
            Op::ANDI(rd, rs, imm) => Psudo::ANDI(rs, rd, imm),
            Op::ORI(rd, rs, imm) => Psudo::ORI(rs, rd, imm),
            Op::XORI(rd, rs, imm) => Psudo::XORI(rs, rd, imm),
            Op::EQI(rd, rs, imm) => Psudo::EQI(rs, rd, imm),
            Op::NEQI(rd, rs, imm) => Psudo::NEQI(rs, rd, imm),
            Op::LTI(rd, rs, imm) => Psudo::LTI(rs, rd, imm),
            Op::LTSI(rd, rs, imm) => Psudo::LTSI(rs, rd, imm),
            Op::LOAD(rd, rs, imm) => Psudo::LOAD(rs, rd, imm),
            Op::STORE(rd, rs, imm) => Psudo::STORE(rs, rd, imm),
            Op::CTRL(rd, rs1, rs2, imm) => {
                if rs2 != 0 {
                    return Psudo::IF(rs2, imm);
                }
                if rs1 == Reg::IRA && rs2 == 0 && rd == 0 {
                    return Psudo::IRET;
                }
                if rs1 == Reg::RA && rs2 == 0 && rd == 0 {
                    return Psudo::RET;
                }
                if rd == Reg::RA {
                    return Psudo::CALL(imm);
                }
                if rs1 == 0 && rs2 == 0 {
                    return Psudo::JUMP(imm);
                }
                if rd == 0 {
                    return Psudo::IF(rs2, imm);
                }
                Psudo::CTRL(rd, rs1, rs2, imm)
            }
        }
    }
}
