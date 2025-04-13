use arch::reg::Reg;
use serde::{Deserialize, Serialize};
use serde_yaml;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;

use crate::model::State;

use super::Hook;

#[derive(Debug)]
pub struct Dump {
    file: Option<String>,
    all: bool,
    list: List,
}

#[derive(Debug, Serialize, Deserialize)]
struct List(HashMap<u16, Config>);

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    stack: bool,
    heap: Vec<u16>,
}

impl Dump {
    pub fn arg(file: Option<String>, all: bool) -> Self {
        match file {
            Some(fname) => {
                let file = File::open(&fname).unwrap();
                let list: List = serde_yaml::from_reader(BufReader::new(file)).unwrap();
                Self {
                    file: Some(fname.clone()),
                    list,
                    all,
                }
            }
            None => Self {
                file,
                list: List(HashMap::new()),
                all,
            },
        }
    }

    fn get(&self, pc: u16) -> Option<&Config> {
        self.list.0.get(&pc)
    }
}

impl Hook for Dump {
    fn init(&mut self, state: State) -> State {
        if self.all {
            println!(" * Dump all");
        }
        if let Some(fname) = &self.file {
            println!(" * Dump[{}] {:?}", self.list.0.len(), fname);
        }
        state
    }
    fn exec(&mut self, _time: u64, addr: u16, _code: u32, cpu: State) -> State {
        if let Some(cfg) = self.get(addr) {
            self.print_reg(&cpu);
            if cfg.stack {
                self.print_stack(&cpu);
            }
            self.print_heap(&cpu, &cfg.heap);
        } else if self.all {
            self.print_reg(&cpu);
        }
        cpu
    }
}

impl Dump {
    fn print_reg(&self, cpu: &State) {
        println!(" +------------+----------+----------+----------+");
        println!(
            " | zero: {:0>4X} | ra: {:0>4X} | t0: {:0>4X} | s0: {:0>4X} |",
            cpu.get(Reg::Z),
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
                " | {:0>4X} : {:0>4X}                                 |",
                sp,
                cpu.get(sp)
            );
        }
        println!(" +---------------------------------------------+");
    }

    fn print_heap(&self, cpu: &State, addrs: &Vec<u16>) {
        for addr in addrs {
            println!(
                " | {:0>4X} : {:0>4X}                                 |",
                addr,
                cpu.get(*addr)
            );
        }
        println!(" +---------------------------------------------+");
    }
}
