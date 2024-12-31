use crate::{alu::ALU, op::Op, reg::Reg};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inst {
    ADD(Reg, Reg, Reg),
    ADDI(Reg, Reg, u16),
    SUB(Reg, Reg, Reg),
    SUBI(Reg, Reg, u16),

    NOT(Reg, Reg),
    AND(Reg, Reg, Reg),
    ANDI(Reg, Reg, u16),
    OR(Reg, Reg, Reg),
    ORI(Reg, Reg, u16),
    XOR(Reg, Reg, Reg),
    XORI(Reg, Reg, u16),

    EQ(Reg, Reg, Reg),
    EQI(Reg, Reg, u16),
    NEQ(Reg, Reg, Reg),
    NEQI(Reg, Reg, u16),
    LT(Reg, Reg, Reg),
    LTI(Reg, Reg, u16),
    LTS(Reg, Reg, Reg),
    LTSI(Reg, Reg, u16),

    SR(Reg, Reg),
    SRS(Reg, Reg),
    SRR(Reg, Reg),
    SL(Reg, Reg),
    SLR(Reg, Reg),

    MOV(Reg, Reg),
    LOAD(Reg, Reg, u16),
    LOADI(Reg, u16),
    STORE(Reg, Reg, u16),

    NOP,
    IF(Reg, u16),
    IFR(Reg, u16),
    JUMP(u16),
    JUMPR(u16),
    CALL(u16),
    RET,
    IRET,
}

impl Inst {
    pub fn to_op(self) -> Op {
        match self {
            Inst::ADD(rs1, rs2, rd) => Op::CALC(ALU::ADD, rd, rs1, rs2),
            Inst::ADDI(rs, rd, imm) => Op::CALCI(ALU::ADD, rd, rs, imm),
            Inst::SUB(rs1, rs2, rd) => Op::CALC(ALU::SUB, rd, rs1, rs2),
            Inst::SUBI(rs, rd, imm) => Op::CALCI(ALU::SUB, rd, rs, imm),

            Inst::NOT(rs, rd) => Op::CALCI(ALU::XOR, rd, rs, 0xFF),
            Inst::AND(rs1, rs2, rd) => Op::CALC(ALU::AND, rd, rs1, rs2),
            Inst::ANDI(rs, rd, imm) => Op::CALCI(ALU::AND, rd, rs, imm),
            Inst::OR(rs1, rs2, rd) => Op::CALC(ALU::OR, rd, rs1, rs2),
            Inst::ORI(rs, rd, imm) => Op::CALCI(ALU::OR, rd, rs, imm),
            Inst::XOR(rs1, rs2, rd) => Op::CALC(ALU::XOR, rd, rs1, rs2),
            Inst::XORI(rs, rd, imm) => Op::CALCI(ALU::XOR, rd, rs, imm),

            Inst::EQ(rs1, rs2, rd) => Op::CALC(ALU::EQ, rd, rs1, rs2),
            Inst::EQI(rs, rd, imm) => Op::CALCI(ALU::EQ, rd, rs, imm),
            Inst::NEQ(rs1, rs2, rd) => Op::CALC(ALU::NEQ, rd, rs1, rs2),
            Inst::NEQI(rs, rd, imm) => Op::CALCI(ALU::NEQ, rd, rs, imm),
            Inst::LT(rs1, rs2, rd) => Op::CALC(ALU::LT, rd, rs1, rs2),
            Inst::LTI(rs, rd, imm) => Op::CALCI(ALU::LT, rd, rs, imm),
            Inst::LTS(rs1, rs2, rd) => Op::CALC(ALU::LTS, rd, rs1, rs2),
            Inst::LTSI(rs, rd, imm) => Op::CALCI(ALU::LTS, rd, rs, imm),

            Inst::SR(rs, rd) => Op::CALC(ALU::SR, rd, rs, Reg::Z),
            Inst::SRS(rs, rd) => Op::CALC(ALU::SRS, rd, rs, Reg::Z),
            Inst::SRR(rs, rd) => Op::CALC(ALU::SRR, rd, rs, Reg::Z),
            Inst::SL(rs, rd) => Op::CALC(ALU::SL, rd, rs, Reg::Z),
            Inst::SLR(rs, rd) => Op::CALC(ALU::SLR, rd, rs, Reg::Z),

            Inst::MOV(rs, rd) => Op::CALC(ALU::ADD, rd, rs, Reg::Z),
            Inst::LOAD(rs, rd, imm) => Op::LOAD(rd, rs, imm),
            Inst::LOADI(rd, imm) => Op::LOAD(rd, Reg::Z, imm),
            Inst::STORE(rs, rd, imm) => Op::STORE(rd, rs, imm),

            Inst::NOP => Op::CALC(ALU::ADD, Reg::Z, Reg::Z, Reg::Z),
            Inst::IF(rs, imm) => Op::CTRL(Reg::Z, rs, Reg::Z, imm),
            Inst::IFR(rs, imm) => Op::CTRL(Reg::Z, rs, Reg::Z, imm),
            Inst::JUMP(imm) => Op::CTRL(Reg::Z, Reg::Z, Reg::Z, imm),
            Inst::JUMPR(imm) => Op::CTRL(Reg::Z, Reg::Z, Reg::Z, imm),
            Inst::CALL(imm) => Op::CTRL(Reg::Z, Reg::Z, Reg::Z, imm),
            Inst::RET => Op::CTRL(Reg::Z, Reg::RA, Reg::Z, 0),
            Inst::IRET => Op::CTRL(Reg::Z, Reg::IRA, Reg::Z, 0),
        }
    }

    pub fn from_op(op: Op) -> Inst {
        match op {
            Op::CALC(alu, rd, rs1, rs2) => match alu {
                ALU::ADD => {
                    if rs2 == Reg::Z {
                        return Inst::MOV(rs1, rd);
                    }
                    Inst::ADD(rs1, rs2, rd)
                }
                ALU::SUB => Inst::SUB(rs1, rs2, rd),
                ALU::AND => Inst::AND(rs1, rs2, rd),
                ALU::OR => Inst::OR(rs1, rs2, rd),
                ALU::XOR => Inst::XOR(rs1, rs2, rd),
                ALU::EQ => Inst::EQ(rs1, rs2, rd),
                ALU::NEQ => Inst::NEQ(rs1, rs2, rd),
                ALU::LT => Inst::LT(rs1, rs2, rd),
                ALU::LTS => Inst::LTS(rs1, rs2, rd),
                ALU::SR => Inst::SR(rs1, rd),
                ALU::SRS => Inst::SRS(rs1, rd),
                ALU::SRR => Inst::SRR(rs1, rd),
                ALU::SL => Inst::SL(rs1, rd),
                ALU::SLR => Inst::SLR(rs1, rd),
            },
            Op::CALCI(alu, rd, rs1, imm) => match alu {
                ALU::ADD => {
                    if rs1 == Reg::Z {
                        return Inst::LOADI(rd, imm);
                    }
                    Inst::ADDI(rs1, rd, imm)
                }
                ALU::SUB => Inst::SUBI(rs1, rd, imm),
                ALU::AND => Inst::ANDI(rs1, rd, imm),
                ALU::OR => Inst::ORI(rs1, rd, imm),
                ALU::XOR => Inst::XORI(rs1, rd, imm),
                ALU::EQ => Inst::EQI(rs1, rd, imm),
                ALU::NEQ => Inst::NEQI(rs1, rd, imm),
                ALU::LT => Inst::LTI(rs1, rd, imm),
                ALU::LTS => Inst::LTSI(rs1, rd, imm),
                ALU::SR => Inst::SR(rs1, rd),
                ALU::SRS => Inst::SRS(rs1, rd),
                ALU::SRR => Inst::SRR(rs1, rd),
                ALU::SL => Inst::SL(rs1, rd),
                ALU::SLR => Inst::SLR(rs1, rd),
            },
            Op::LOAD(rd, rs, imm) => Inst::LOAD(rs, rd, imm),
            Op::STORE(rd, rs, imm) => Inst::STORE(rs, rd, imm),
            Op::CTRL(rd, rs1, rs2, imm) => {
                if rs2 != Reg::Z {
                    return Inst::IF(rs2, imm);
                }
                if rs1 == Reg::IRA && rs2 == Reg::Z && rd == Reg::Z {
                    return Inst::IRET;
                }
                if rs1 == Reg::RA && rs2 == Reg::Z && rd == Reg::Z {
                    return Inst::RET;
                }
                if rd == Reg::RA {
                    return Inst::CALL(imm);
                }
                if rs1 == Reg::Z && rs2 == Reg::Z {
                    return Inst::JUMP(imm);
                }
                if rd == Reg::Z {
                    return Inst::IF(rs2, imm);
                }
                panic!("Undefined Inst: {:?}", op);
            }
        }
    }
}
