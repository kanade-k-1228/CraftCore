mod symbols;
mod table;

pub use symbols::{Symbol, Symbols};
pub use table::{
    asms::{AsmEntry, AsmMap},
    consts::{ConstEntry, ConstMap},
    funcs::{FuncEntry, FuncMap},
    statics::{StaticEntry, StaticMap},
    types::{TypeEntry, TypeMap},
};
