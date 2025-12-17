// Code generation module

pub mod asm2code;
pub mod func2code;
pub mod types;

pub use asm2code::asm2code;
pub use func2code::func2code;
pub use types::Code;
