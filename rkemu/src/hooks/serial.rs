use crate::computer::{Hook, State};

pub struct Serial {}

impl Serial {
    const TX: u16 = 0x1000;
    pub fn new() -> Self {
        Serial {}
    }
}

impl Hook for Serial {
    fn exec(&mut self, _time: u64, _addr: u16, _code: u32, state: State) -> State {
        let stdout = state.ram.get(Self::TX);
        if stdout != u16::MAX {
            println!("> {}", stdout as u8 as char);
        }
        state
    }
}
