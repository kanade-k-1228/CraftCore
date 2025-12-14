use std::collections::HashMap;

use crate::fir::{Func, FIR};
use crate::global::{Global, Globals};

pub fn flowgen(globals: &Globals) -> Result<FIR, String> {
    let mut fir = Vec::new();
    for (name, global) in globals.0.iter() {
        match global {
            Global::Func(args, ret, body) => {
                let func = Func {
                    name: name.clone(),
                    nodes: HashMap::new(),
                };
                fir.push(func);
            }
            _ => {}
        }
    }
    Ok(FIR(fir))
}
