use std::fs::File;
use std::io::{self, Read};

pub struct State {
    rom: Vec<u32>,
    pub ram: RAM,
    pub shutdown: bool,
}

pub trait Hook {
    fn exec(&mut self, time: u64, addr: u16, code: u32, state: State) -> State;
}

impl State {
    pub fn new() -> Self {
        State {
            rom: vec![0; 1024], // Adjust the size as needed
            ram: RAM::new(),
            shutdown: false,
        }
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

    pub fn interrupt(&self, irq_flag: u16) {
        static mut IRQ_FLAG_LATCH: u16 = 0;
        const IRQ_0: u16 = 1;
        const IRQ_1: u16 = 2;
        const IRQ_2: u16 = 4;
        const IRQ_3: u16 = 8;

        // if self.ram.get(CSR::IRQ_EN) != 0 {
        //     unsafe {
        //         if IRQ_FLAG_LATCH & (IRQ_0 | IRQ_1 | IRQ_2 | IRQ_3) != 0 {
        //             self.ram.set_ira();
        //             self.ram.set_pc(Addr::PC_INTR);
        //         }
        //         IRQ_FLAG_LATCH = irq_flag;
        //     }
        // }
    }

    pub fn exec(&mut self) -> (u16, u32) {
        let pc = self.ram.get(1);
        let code = *self.rom.get(pc as usize).unwrap();
        let op = Decoder::new(code);
        match op.opc {
            OPCode::Calc => self.calc(op.func, op.rd, op.rs1, op.rs2),
            OPCode::Calci => self.calci(op.func, op.rd, op.rs1, op.imm),
            OPCode::Load => self.load(op.rd, op.rs1, op.imm),
            OPCode::Store => self.store(op.rs1, op.rs2, op.imm),
            OPCode::Ctrl => self.ctrl(op.rd, op.rs1, op.rs2, op.imm),
        }
        return (pc, code);
    }

    fn calc(&mut self, alu_func: u8, rd: u16, rs1: u16, rs2: u16) {
        self.ram
            .set(rd, alu(alu_func, self.ram.get(rs1), self.ram.get(rs2)));
        self.ram.inc_pc();
    }

    fn calci(&mut self, alu_func: u8, rd: u16, rs1: u16, imm: u16) {
        self.ram.set(rd, alu(alu_func, self.ram.get(rs1), imm));
        self.ram.inc_pc();
    }

    fn load(&mut self, rd: u16, rs1: u16, imm: u16) {
        self.ram.set(rd, self.ram.get(self.ram.get(rs1) + imm));
        self.ram.inc_pc();
    }

    fn store(&mut self, rs1: u16, rs2: u16, imm: u16) {
        self.ram.set(self.ram.get(rs1) + imm, self.ram.get(rs2));
        self.ram.inc_pc();
    }

    fn ctrl(&mut self, rd: u16, rs1: u16, rs2: u16, imm: u16) {
        self.ram.set(rd, self.ram.get(1) + 1);
        if self.ram.get(rs2) == 0 {
            self.ram.set_pc(self.ram.get(rs1) + imm);
        } else {
            self.ram.inc_pc();
        }
    }
}

// Placeholder structs and enums for the missing parts
pub struct RAM;
impl RAM {
    fn new() -> Self {
        RAM
    }
    pub fn get(&self, _addr: u16) -> u16 {
        0
    }
    fn set(&mut self, _addr: u16, _value: u16) {}
    fn inc_pc(&mut self) {}
    fn set_pc(&mut self, _addr: u16) {}
    fn set_ira(&mut self) {}
}

struct Decoder {
    opc: OPCode,
    func: u8,
    rd: u16,
    rs1: u16,
    rs2: u16,
    imm: u16,
}
impl Decoder {
    fn new(_code: u32) -> Self {
        Decoder {
            opc: OPCode::Calc,
            func: 0,
            rd: 0,
            rs1: 0,
            rs2: 0,
            imm: 0,
        }
    }
}

enum OPCode {
    Calc,
    Calci,
    Load,
    Store,
    Ctrl,
}

enum Serial {
    TX,
}

enum CSR {
    IRQ_EN,
}

enum Addr {
    PC_INTR,
}

enum Reg {
    PC,
}

fn alu(_func: u8, _a: u16, _b: u16) -> u16 {
    0
}
