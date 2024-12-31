use arch::reg::Reg;
use serde::{Deserialize, Serialize};
use serde_yaml;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::BufReader;

use crate::model::State;

use super::Hook;

#[derive(Debug, Serialize, Deserialize)]
pub struct Print {
    dumps: HashMap<u16, Dump>,
    print_all: bool,
}

#[derive(Debug, Serialize, Deserialize)]
struct Dump {
    stack: bool,
    heap: HashSet<u16>,
}

impl Print {
    pub fn arg(file: Option<String>, print_all: bool) -> Self {
        match file {
            Some(fname) => Self::from_file(&fname),
            None => Self {
                dumps: HashMap::new(),
                print_all,
            },
        }
    }

    fn from_file(path: &str) -> Self {
        let file = File::open(path).unwrap();
        serde_yaml::from_reader(BufReader::new(file)).unwrap()
    }
}

impl Print {
    fn print_reg(&self, cpu: &State) {
        println!(" +------------+----------+----------+----------+");
        println!(
            " | zero: {:0>4X} | ra: {:0>4X} | t0: {:0>4X} | s0: {:0>4X} |",
            cpu.get(Reg::ZERO),
            cpu.get(Reg::RA),
            cpu.get(Reg::T0),
            cpu.get(Reg::S0)
        );
        println!(
            " |  ira: {:0>4X} | fp: {:0>4X} | t1: {:0>4X} | s1: {:0>4X} |",
            cpu.get(Reg::IRA),
            cpu.get(Reg::FP),
            cpu.get(Reg::T1),
            cpu.get(Reg::S1)
        );
        println!(
            " |   pc: {:0>4X} | a0: {:0>4X} | t2: {:0>4X} | s2: {:0>4X} |",
            cpu.get(Reg::PC),
            cpu.get(Reg::A0),
            cpu.get(Reg::T2),
            cpu.get(Reg::S2)
        );
        println!(
            " |   sp: {:0>4X} | a1: {:0>4X} | t3: {:0>4X} | s3: {:0>4X} |",
            cpu.get(Reg::SP),
            cpu.get(Reg::A1),
            cpu.get(Reg::T3),
            cpu.get(Reg::S3)
        );
        println!(" +------------+----------+----------+----------+");
    }
    fn print_stack(&self, cpu: &State) {
        let sp = cpu.get(Reg::SP);
        let fp = cpu.get(Reg::FP);
        for sp in sp..fp {
            println!(
                " | {:0>4x} : {:0>4x}                                 |",
                sp,
                cpu.get(sp)
            );
        }
        println!(" +---------------------------------------------+");
    }
}

impl Hook for Print {
    fn init(&mut self, state: State) -> State {
        if self.print_all {
            println!("* Print: all");
        }
        state
    }
    fn exec(&mut self, _time: u64, addr: u16, _code: u32, state: State) -> State {
        if self.print_all {
            self.print_reg(&state);
            self.print_stack(&state);
        }
        if let Some(dump) = self.dumps.get(&addr) {
            println!("Dump at 0x{:0>4X}:", addr);
            // println!("  - Stack: {:?}", cpu.ram.stack);
            for addr in dump.heap.iter() {
                println!("  - 0x{:0>4X}: 0x{:0>4X}", addr, state.get(*addr));
            }
        }
        state
    }
}
