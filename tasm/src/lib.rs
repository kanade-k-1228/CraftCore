pub mod cli;
mod error;
mod eval;
mod grammer;
mod linker;
mod util;

pub use error::Error;
pub use eval::{
    asm,
    code::{Code, Imm},
    constexpr::ConstExpr,
    func,
    global::{apply_module_prefixes, Global},
    normtype::NormType,
};
pub use grammer::ast;
pub use grammer::lexer::Lexer;
pub use grammer::parsercore::Parser;
pub use grammer::token::{Pos, Token, TokenKind};
pub use linker::allocator::Allocator;
pub use linker::binary::{gencbin, genibin};
pub use linker::memory::Memory;
pub use util::display::binprint;
pub use util::maps::SymbolMap;
pub use util::modules::{collect_module_files, parse_include_spec};
