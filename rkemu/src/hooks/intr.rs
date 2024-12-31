use serde::{Deserialize, Serialize};
use serde_yaml;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;

use crate::model::State;

use super::Hook;

#[derive(Debug, Serialize, Deserialize)]
pub struct Intr {
    file: Option<String>,
    events: Events,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Events(HashMap<i32, i32>);

impl Intr {
    pub fn arg(file: Option<String>) -> Self {
        match file {
            Some(fname) => {
                let file = File::open(&fname).unwrap();
                let events: Events = serde_yaml::from_reader(BufReader::new(file)).unwrap();
                Self {
                    file: Some(fname.clone()),
                    events,
                }
            }
            None => Self {
                file: None,
                events: Events(HashMap::new()),
            },
        }
    }

    fn get(&self, time: u16) -> Option<(&i32, &i32)> {
        self.events.0.get_key_value(&(time as i32))
    }
}

impl Hook for Intr {
    fn init(&mut self, state: State) -> State {
        if let Some(fname) = &self.file {
            println!(
                "* Intr: Initialized: {:?} # {:?}",
                fname,
                self.events.0.len()
            );
        }
        state
    }
    fn exec(&mut self, time: u64, _addr: u16, _code: u32, mut cpu: State) -> State {
        if let Some((t, i)) = self.get(time as u16) {
            println!("* Intr: at {} # {}", t, i);
            cpu.interrupt();
        }
        cpu
    }
}
