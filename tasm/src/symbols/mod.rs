mod symbols;
mod table;

pub use symbols::Symbols;
pub use table::{
    asms::AsmEntry,
    consts::{ConstEntry, ConstMap},
    funcs::{FuncEntry, FuncMap},
    statics::{StaticEntry, StaticMap},
    types::{TypeEntry, TypeMap},
};
