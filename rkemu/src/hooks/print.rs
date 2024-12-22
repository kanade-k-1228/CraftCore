use serde::{Deserialize, Serialize};
use serde_yaml;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::BufReader;

use crate::computer::{Hook, State};

#[derive(Debug, Serialize, Deserialize)]
pub struct Print {
    dumps: HashMap<u16, Dump>,
    print_all: bool,
}

#[derive(Debug, Serialize, Deserialize)]
struct Dump {
    stack: bool,
    address: HashSet<u16>,
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
    fn is_dump(&self, pc: u16) -> bool {
        self.dumps.contains_key(&pc)
    }

    fn print_op(&self, pc: u16, cpu: &State) {
        println!("0x{:04x}: {:?}", pc, cpu.ram.get(pc));
    }
    fn print_reg(&self, cpu: &State) {
        // println!("  - Regs: {:?}", cpu.ram.regs);
    }
    fn print_stack(&self, cpu: &State) {
        // println!("  - Stack: {:?}", cpu.ram.stack);
    }
}

impl Hook for Print {
    fn exec(&mut self, _time: u64, addr: u16, _code: u32, state: State) -> State {
        if self.is_dump(addr) {
            println!("Dump at 0x{:04x}:", addr);
            // println!("  - Stack: {:?}", cpu.ram.stack);
            for addr in self.dumps.get(&addr).unwrap().address.iter() {
                println!("  - 0x{:04x}: 0x{:04x}", addr, state.ram.get(*addr));
            }
        }
        state
    }
}
