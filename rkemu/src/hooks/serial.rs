use super::Hook;
use crate::model::State;

pub struct Serial {}

impl Serial {
    const TX: u16 = 0x1000;
    const NONE: u16 = 0xFFFF;
    pub fn new() -> Self {
        Serial {}
    }
}

impl Hook for Serial {
    fn init(&mut self, mut state: State) -> State {
        state.set(Self::TX, Serial::NONE);
        println!(" * Serial: TX={:0>4X}", state.get(Self::TX));
        state
    }
    fn exec(&mut self, _time: u64, _addr: u16, _code: u32, state: State) -> State {
        let stdout = state.get(Self::TX);
        if stdout != Serial::NONE {
            println!("> {:?}", stdout as u8 as char);
        }
        state
    }
}
