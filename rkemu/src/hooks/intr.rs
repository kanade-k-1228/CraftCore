use serde::{Deserialize, Serialize};
use serde_yaml;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;

use crate::computer::{Hook, State};

#[derive(Debug, Serialize, Deserialize)]
pub struct Intr {
    events: HashMap<i32, Irq>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Irq {
    irq_flag: i32,
}

impl Intr {
    pub fn arg(file: Option<String>) -> Self {
        match file {
            Some(fname) => Self::from_file(&fname),
            None => Self::new(),
        }
    }

    fn new() -> Self {
        Self {
            events: HashMap::new(),
        }
    }

    fn from_file(path: &str) -> Self {
        let file = File::open(path).unwrap();
        serde_yaml::from_reader(BufReader::new(file)).unwrap()
    }

    fn is_intr(&self, time: u16) -> bool {
        self.events.contains_key(&(time as i32))
    }
}

impl Hook for Intr {
    fn exec(&mut self, time: u64, _addr: u16, _code: u32, state: State) -> State {
        if self.is_intr(time as u16) {
            state.interrupt(self.events[&(time as i32)].irq_flag as u16);
        }
        state
    }
}
