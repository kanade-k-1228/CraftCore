use color_print::cprintln;

use super::Hook;
use crate::model::State;

pub struct Serial {
    style: bool,
    tx_buf: Vec<char>,
}

impl Serial {
    const TX: u16 = 0x1000;
    const RX: u16 = 0x1001;
    const NONE: u16 = 0xFFFF;
    pub fn arg(style: bool, tx_buf: Vec<char>) -> Serial {
        Serial { style, tx_buf }
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
                true => cprintln!("   > <s>{:?}</>", c),
                false => print!("{}", c),
            }
        }
        state.set(Serial::TX, Serial::NONE);
        state
    }
}
