pub mod intr;
pub mod print;
pub mod serial;
pub trait Hook {
    fn init(&mut self, cpu: crate::model::State) -> crate::model::State;
    fn exec(
        &mut self,
        time: u64,
        addr: u16,
        code: u32,
        cpu: crate::model::State,
    ) -> crate::model::State;
}
