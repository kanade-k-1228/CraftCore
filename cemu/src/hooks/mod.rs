pub mod dump;
pub mod intr;
pub mod serial;
pub mod video;

use crate::model::State;

pub trait Hook {
    fn init(&mut self, cpu: State) -> State;
    fn exec(&mut self, time: u64, addr: u16, code: u32, cpu: State) -> State;
}
