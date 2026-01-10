use serde::{Deserialize, Serialize};
use serde_yaml;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;

use crate::model::State;

use super::Hook;

#[derive(Debug)]
pub struct Intr {
    file: Option<String>,
    list: List,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct List(HashMap<i32, i32>);

const INDENT: usize = 32;

impl Intr {
    pub fn arg(file: Option<String>) -> Self {
        match file {
            Some(fname) => {
                let file = File::open(&fname).unwrap();
                let list: List = serde_yaml::from_reader(BufReader::new(file)).unwrap();
                Self {
                    file: Some(fname.clone()),
                    list,
                }
            }
            None => Self {
                file: None,
                list: List(HashMap::new()),
            },
        }
    }

    fn get(&self, time: u16) -> Option<&i32> {
        self.list.0.get(&(time as i32))
    }
}

impl Hook for Intr {
    fn init(&mut self, state: State) -> State {
        if let Some(fname) = &self.file {
            println!(" * Intr[{}] {:?}", self.list.0.len(), fname);
        }
        state
    }
    fn exec(&mut self, time: u64, _: u16, _: u32, mut cpu: State) -> State {
        if let Some(intr) = self.get(time as u16) {
            println!("\x1b[1A\x1b[{}C!{}", INDENT, intr);
            cpu.interrupt();
        }
        cpu
    }
}
