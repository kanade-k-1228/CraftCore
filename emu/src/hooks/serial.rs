use std::io::Read;

use color_print::cprintln;

use super::Hook;
use crate::model::State;

pub struct Serial {
    style: bool,
    read_buf: Vec<char>,
    write_buf: Box<dyn std::io::Write>,
}

impl Serial {
    const TX: u16 = 0x1000;
    const RX: u16 = 0x1001;
    const NONE: u16 = 0xFFFF;
    pub fn arg(style: bool, write_file: Option<String>, read_file: Option<String>) -> Serial {
        // open file and read
        let read_buf = match read_file {
            Some(file) => {
                let mut buf = Vec::new();
                let mut file = std::fs::File::open(file).unwrap();
                file.read_to_end(&mut buf).unwrap();
                buf.into_iter().map(|b| b as char).collect()
            }
            None => vec![],
        };

        // open file and create writer buffer
        let write_buf = match write_file {
            Some(file) => {
                let file = std::fs::File::create(file).unwrap();
                Box::new(std::io::BufWriter::new(file)) as Box<dyn std::io::Write>
            }
            None => Box::new(std::io::BufWriter::new(std::io::stdout())) as Box<dyn std::io::Write>,
        };

        Serial {
            style,
            read_buf,
            write_buf,
        }
    }
}

impl Hook for Serial {
    fn init(&mut self, mut state: State) -> State {
        state.set(Serial::TX, Serial::NONE);
        state.set(Serial::RX, Serial::NONE);
        println!(
            " * Serial: TX={:0>4X} RX={:0>4X}",
            state.get(Serial::TX),
            state.get(Serial::RX)
        );
        state
    }
    fn exec(&mut self, _time: u64, _addr: u16, _code: u32, mut state: State) -> State {
        let stdout = state.get(Serial::TX);
        if stdout != Serial::NONE {
            let c = stdout as u8 as char;
            match self.style {
                true => cprintln!("   > <r,s>{:?}</>", c),
                false => print!("{}", c),
            }
            self.write_buf.write_all(&[c as u8]).unwrap();
        }
        if let Some(c) = self.read_buf.pop() {
            state.set(Serial::RX, c as u16);
        } else {
            state.set(Serial::RX, Serial::NONE);
        }
        state.set(Serial::TX, Serial::NONE);
        state
    }
}

impl Drop for Serial {
    fn drop(&mut self) {
        self.write_buf.flush().unwrap();
    }
}
