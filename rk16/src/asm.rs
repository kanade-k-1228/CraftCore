use std::collections::HashMap;

use once_cell::sync::Lazy;

enum Arg {
    Reg,
    Imm,
}

enum Field {
    Arg(usize),
    Const(usize),
}

struct Asm {
    name: &'static str,
    args: Vec<Arg>,
    field: Vec<(i32, i32)>,
}

const Asm: Lazy<HashMap<&'static str, (Vec<Arg>, Vec<(i32, i32)>)>> = Lazy::new(|| {
    let rrr = || vec![Arg::Reg, Arg::Reg, Arg::Reg];
    let frrr = || vec![(16, 0), (4, 0), (4, 0), (4, 0)];
    let mut map: HashMap<&'static str, (Vec<Arg>, Vec<(i32, i32)>)> = HashMap::new();
    map.insert("add", (rrr(), frrr()));
    map
});
