use std::fs::File;
use std::io::{self, Read};
use std::u16;

use arch::alu::{alu_model, ALU};
use arch::inst::Inst;
use arch::op::Op;
use arch::reg::Reg;

pub struct State {
    rom: Vec<u32>,
    ram: Vec<u16>,
}

// Memory access
impl State {
    pub fn get(&self, addr: impl Into<u16>) -> u16 {
        self.ram[addr.into() as usize]
    }

    pub fn set(&mut self, addr: impl Into<u16>, val: u16) {
        let addr = addr.into() as usize;
        if addr != 0 {
            self.ram[addr] = val;
        }
    }

    fn inc_pc(&mut self) {
        self.ram[Reg::PC as usize] += 1;
    }
    fn set_pc(&mut self, val: u16) {
        self.ram[Reg::PC as usize] = val;
    }
}

impl State {
    const INTR_ADDR: u16 = 0x0001;
    pub fn interrupt(&mut self) {
        self.ram[Reg::PC as usize] = Self::INTR_ADDR;
    }
}

impl State {
    pub fn new() -> Self {
        State {
            rom: vec![0; 65536],
            ram: vec![0; 65536],
        }
    }

    pub fn is_terminated(&self) -> bool {
        false
    }

    pub fn load_rom_file(&mut self, fname: &str) -> io::Result<()> {
        let mut file = File::open(fname)?;
        let mut buf = [0u8; 4];
        let mut op_cnt = 0;
        while file.read_exact(&mut buf).is_ok() {
            let word = u32::from_le_bytes(buf);
            self.rom[op_cnt] = word;
            op_cnt += 1;
        }
        Ok(())
    }

    pub fn exec(&mut self, time: u64) -> (u16, u32) {
        let pc = self.ram[Reg::PC as usize];
        let bin = self.rom[pc as usize];
        let op = Op::from_bin(bin);
        let inst = Inst::from_op(op.clone());

        println!("[{:0>4}] {:?}", time, inst);

        match op {
            Op::CALC(alu, rd, rs1, rs2) => self.calc(alu, rd, rs1, rs2),
            Op::CALCI(alu, rd, rs1, imm) => self.calci(alu, rd, rs1, imm),
            Op::LOAD(rd, rs1, imm) => self.load(rd, rs1, imm),
            Op::STORE(rs1, rs2, imm) => self.store(rs1, rs2, imm),
            Op::CTRL(rd, rs1, rs2, imm) => self.ctrl(rd, rs1, rs2, imm),
        };
        return (pc, bin);
    }

    fn calc(&mut self, alu: ALU, rd: Reg, rs1: Reg, rs2: Reg) {
        let calc = alu_model(alu, self.get(rs1), self.get(rs2));
        self.set(rd, calc);
        self.inc_pc();
    }

    fn calci(&mut self, alu: ALU, rd: Reg, rs1: Reg, imm: u16) {
        let calc = alu_model(alu, self.get(rs1), imm);
        self.set(rd, calc);
        self.inc_pc();
    }

    fn load(&mut self, rd: Reg, rs1: Reg, imm: u16) {
        self.set(rd, self.get(self.get(rs1) + imm));
        self.inc_pc();
    }

    fn store(&mut self, rs1: Reg, rs2: Reg, imm: u16) {
        self.set(self.get(rs1) + imm, self.get(rs2));
        self.inc_pc();
    }

    fn ctrl(&mut self, rd: Reg, rs1: Reg, rs2: Reg, imm: u16) {
        self.set(rd, self.get(Reg::PC) + 1);
        if self.get(rs2) == 0 {
            self.set_pc(self.get(rs1) + imm);
        } else {
            self.inc_pc();
        }
    }
}
