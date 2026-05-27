use std::fs::File;
use std::io::{self, Read};
use std::u16;

use arch::alu::ALU;
use arch::inst::Inst;
use arch::op::Op;
use arch::reg::Reg;

pub struct State {
    imem: Vec<u32>,
    dmem: Vec<u16>,
}

// Memory access
impl State {
    pub fn get(&self, addr: impl Into<u16>) -> u16 {
        self.dmem[addr.into() as usize]
    }

    pub fn set(&mut self, addr: impl Into<u16>, val: u16) {
        let addr = addr.into() as usize;
        if addr != 0 {
            self.dmem[addr] = val;
        }
    }

    fn inc_pc(&mut self) {
        self.dmem[Reg::PC as usize] = self.dmem[Reg::PC as usize].wrapping_add(1);
    }
    fn set_pc(&mut self, val: u16) {
        self.dmem[Reg::PC as usize] = val;
    }
}

// Interrupt
impl State {
    // 割り込みベクタは PC=4 (TASM 側の `asm @ 0x0004 irq` と整合)
    const INTR_ADDR: u16 = 0x0004;
    // CSR の ENABLE ビット (bit 0)
    pub const CSR_ENABLE: u16 = 0x0001;
    pub fn interrupt(&mut self) {
        // ハードウェアと同じく、割り込み受理時は atomic に:
        //   1. 現在 PC を IRA に退避
        //   2. CSR.ENABLE を落とす (多重割り込み防止)
        //   3. PC を割り込みベクタに設定
        let pc = self.dmem[Reg::PC as usize];
        self.dmem[Reg::IRA as usize] = pc;
        let csr = self.dmem[Reg::CSR as usize];
        self.dmem[Reg::CSR as usize] = csr & !Self::CSR_ENABLE;
        self.dmem[Reg::PC as usize] = Self::INTR_ADDR;
    }

    pub fn intr_enabled(&self) -> bool {
        (self.dmem[Reg::CSR as usize] & Self::CSR_ENABLE) != 0
    }

    pub fn csr_set_flag(&mut self, intr_no: u32) {
        let flag = 1u16 << (8 + intr_no);
        let csr = self.dmem[Reg::CSR as usize];
        self.dmem[Reg::CSR as usize] = csr | flag;
    }
}

// Control
impl State {
    const HALT: u16 = 0x0010;

    pub fn halt(&self) -> bool {
        self.dmem[Self::HALT as usize] == 0x0001
    }
}

// const WAIT: u16 = 0x0011;
// const MODE: u16 = 0x0012;
// const IEN: u16 = 0x0013;
// const IMASK: u16 = 0x0014;
// const IRQ: u16 = 0x0015;
// const TRAP: u16 = 0x0016;

impl State {
    pub fn new() -> Self {
        State {
            imem: vec![0; 65536],
            dmem: vec![0; 65536],
        }
    }

    pub fn load_imem(&mut self, fname: &str) -> io::Result<()> {
        let mut file = File::open(fname)?;
        let mut buf = [0u8; 4];
        let mut op_cnt = 0;
        while file.read_exact(&mut buf).is_ok() {
            let word = u32::from_le_bytes(buf);
            self.imem[op_cnt] = word;
            op_cnt += 1;
        }
        Ok(())
    }

    pub fn load_dmem(&mut self, fname: &str) -> io::Result<()> {
        let mut file = File::open(fname)?;
        let mut buf = [0u8; 2];
        let mut addr = 0;
        while file.read_exact(&mut buf).is_ok() {
            let word = u16::from_le_bytes(buf);
            self.dmem[addr] = word;
            addr += 1;
        }
        Ok(())
    }

    pub fn exec(&mut self) -> (u16, u32, Op, Inst<Reg, u16>) {
        let pc = self.dmem[Reg::PC as usize];
        let bin = self.imem[pc as usize];
        let op = Op::from_bin(bin);
        let inst = Inst::from_op(op.clone());

        match op {
            Op::CALC(alu, rd, rs1, rs2) => self.calc(alu, rd, rs1, rs2),
            Op::CALCI(alu, rd, rs1, imm) => self.calci(alu, rd, rs1, imm),
            Op::LOAD(rd, rs1, imm) => self.load(rd, rs1, imm),
            Op::STORE(rs2, rs1, imm) => self.store(rs2, rs1, imm),
            Op::CTRL(rd, rs1, rs2, imm) => self.ctrl(rd, rs1, rs2, imm),
        };
        return (pc, bin, op, inst);
    }

    fn calc(&mut self, alu: ALU, rd: Reg, rs1: Reg, rs2: Reg) {
        self.set(rd, alu.calc(self.get(rs1), self.get(rs2)));
        self.inc_pc();
    }

    fn calci(&mut self, alu: ALU, rd: Reg, rs1: Reg, imm: u16) {
        self.set(rd, alu.calc(self.get(rs1), imm));
        self.inc_pc();
    }

    fn load(&mut self, rd: Reg, rs1: Reg, imm: u16) {
        // imm は 2's complement の符号付きオフセットとして扱うため wrapping_add
        self.set(rd, self.get(self.get(rs1).wrapping_add(imm)));
        self.inc_pc();
    }

    fn store(&mut self, rs2: Reg, rs1: Reg, imm: u16) {
        self.set(self.get(rs1).wrapping_add(imm), self.get(rs2));
        self.inc_pc();
    }

    fn ctrl(&mut self, rd: Reg, rs1: Reg, rs2: Reg, imm: u16) {
        self.set(rd, self.get(Reg::PC).wrapping_add(1));
        // 命令エンコーディング上 rs2 = Z は無条件 (JUMP / JUMPR / CALL / CALLR / RET / IRET)。
        // それ以外は JUMPIF / JUMPIFR で、「rs2 が非ゼロなら飛ぶ」(natural 条件分岐)。
        let take_jump = if rs2 == Reg::Z {
            true
        } else {
            self.get(rs2) != 0
        };
        if take_jump {
            self.set_pc(self.get(rs1).wrapping_add(imm));
        } else {
            self.inc_pc();
        }
    }
}
