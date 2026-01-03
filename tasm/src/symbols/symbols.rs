use indexmap::IndexMap;

use crate::{
    error::CollectError,
    grammer::ast,
    symbols::{
        table::{
            asms::AsmMap, consts::ConstMap, funcs::FuncMap, statics::StaticMap, types::TypeMap,
        },
        AsmEntry, ConstEntry, FuncEntry, StaticEntry, TypeEntry,
    },
};

#[derive(Debug)]
pub enum Symbol<'a> {
    Type(&'a TypeEntry<'a>),
    Const(&'a ConstEntry<'a>),
    Static(&'a StaticEntry<'a>),
    Asm(&'a AsmEntry<'a>),
    Func(&'a FuncEntry<'a>),
}

#[derive(Debug)]
pub struct Symbols<'a> {
    pub types: TypeMap<'a>,
    pub consts: ConstMap<'a>,
    pub statics: StaticMap<'a>,
    pub asms: AsmMap<'a>,
    pub funcs: FuncMap<'a>,
}

impl<'a> Symbols<'a> {
    pub fn collect(ast: &'a ast::AST) -> Result<Self, CollectError> {
        // Collect each table
        let consts = ConstMap::collect(ast)?;
        let types = TypeMap::collect(ast, &consts)?;
        let statics = StaticMap::collect(ast, &consts, &types)?;
        let asms = AsmMap::collect(ast, &consts)?;
        let funcs = FuncMap::collect(ast, &consts, &types, &statics)?;

        // Check for duplicate names across all symbol tables
        let mut all_names = IndexMap::new();

        // Collect types
        for &name in types.0.keys() {
            if let Some(existing) = all_names.insert(name, "Type") {
                return Err(CollectError::Duplicate(format!(
                    "{} (conflicts with {})",
                    name, existing
                )));
            }
        }

        // Collect consts
        for &name in consts.0.keys() {
            if let Some(existing) = all_names.insert(name, "Const") {
                return Err(CollectError::Duplicate(format!(
                    "{} (conflicts with {})",
                    name, existing
                )));
            }
        }

        // Collect statics
        for &name in statics.0.keys() {
            if let Some(existing) = all_names.insert(name, "Static") {
                return Err(CollectError::Duplicate(format!(
                    "{} (conflicts with {})",
                    name, existing
                )));
            }
        }

        // Collect asms
        for &name in asms.0.keys() {
            if let Some(existing) = all_names.insert(name, "Asm") {
                return Err(CollectError::Duplicate(format!(
                    "{} (conflicts with {})",
                    name, existing
                )));
            }
        }

        // Collect funcs
        for &name in funcs.0.keys() {
            if let Some(existing) = all_names.insert(name, "Func") {
                return Err(CollectError::Duplicate(format!(
                    "{} (conflicts with {})",
                    name, existing
                )));
            }
        }

        Ok(Symbols {
            types,
            consts,
            statics,
            asms,
            funcs,
        })
    }

    /// Get a symbol by name
    pub fn get(&self, name: &str) -> Option<Symbol<'_>> {
        if let Some(entry) = self.types.0.get(name) {
            return Some(Symbol::Type(entry));
        }
        if let Some(entry) = self.consts.0.get(name) {
            return Some(Symbol::Const(entry));
        }
        if let Some(entry) = self.statics.0.get(name) {
            return Some(Symbol::Static(entry));
        }
        if let Some(entry) = self.asms.0.get(name) {
            return Some(Symbol::Asm(entry));
        }
        if let Some(entry) = self.funcs.0.get(name) {
            return Some(Symbol::Func(entry));
        }
        None
    }

    /// Get the asms map
    pub fn asms(&self) -> &IndexMap<&'a str, AsmEntry<'a>> {
        &self.asms.0
    }

    /// Get the consts map
    pub fn consts(&self) -> &IndexMap<&'a str, ConstEntry<'a>> {
        &self.consts.0
    }

    /// Get the statics map
    pub fn statics(&self) -> &IndexMap<&'a str, StaticEntry<'a>> {
        &self.statics.0
    }

    /// Get the funcs map
    pub fn funcs(&self) -> &IndexMap<&'a str, FuncEntry<'a>> {
        &self.funcs.0
    }

    /// Get the types map
    pub fn types(&self) -> &IndexMap<&'a str, TypeEntry<'a>> {
        &self.types.0
    }
}
