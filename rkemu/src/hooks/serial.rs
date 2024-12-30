use crate::model::State;

pub struct Serial {}

impl Serial {
    const TX: u16 = 0x1000;
    pub fn new() -> Self {
        Serial {}
    }
}

impl super::Hook for Serial {
    fn init(&mut self, mut state: State) -> State {
        state.set(Self::TX, u16::MAX);
        println!("Serial initialized: TX={:0>4X}", state.get(Self::TX));
        state
    }
    fn exec(&mut self, _time: u64, _addr: u16, _code: u32, state: State) -> State {
        let stdout = state.get(Self::TX);
        if stdout != u16::MAX {
            println!("> {:?}", stdout as u8 as char);
        }
        state
    }
}
