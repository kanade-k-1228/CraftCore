use tasm::lexer::LineLexer;
use tasm::parser::Parser;
use tasm::token::{Token, TokenKind};

fn assert(code: &str) {
    let mut tokens: Vec<Token> = Vec::new();
    for (line_idx, line) in code.lines().enumerate() {
        println!("{:>2}: {}", line_idx, line);
        let toks = LineLexer::new(code, 0, 0).parse();
        for tok in &toks {
            print!("\r\x1b[{}C^", tok.pos.row + 4);
        }
        println!();
        for (i, tok) in toks.iter().enumerate() {
            println!("{:>2}: {:?}", i, tok.kind);
        }
        tokens.extend(
            toks.into_iter()
                .filter(|t| !matches!(t.kind, TokenKind::Comment(_))),
        );
    }

    let (ast, errors) = Parser::new(tokens.into_iter()).parse();
    println!("{:#?}", ast);
    for error in &errors {
        println!("{:?}", error);
    }
    assert!(errors.is_empty(), "Errors found: {:?}", errors);
}

macro_rules! case {
    ($name:ident, $code:expr) => {
        #[test]
        fn $name() {
            assert($code);
        }
    };
}

case!(type_def_0, "type Alias = CustomType;");

case!(type_pointer_0, "type Ptr = *int;");
case!(type_array_0, "type Array = [4]int;");
case!(type_func_0, "type Func = (a: int, b: int) -> int;");
case!(type_struct_0, "type Struct = { x: int, y: int };");

case!(const_0, "const a = { a = 0, b = 1 };");
case!(const_1, "const a = [0, 1];");

// Static variables
case!(static_no_addr, "static data: [10]int;");
case!(static_with_addr, "static @ 16 foo: int;");

// Assembly
case!(asm_0, "asm irq @ 0x0004 { t0 = add(t0, 0x0001); }");

// Function
case!(func_args, "fn add(a: int, b: int) -> int { return a + b; }");
case!(func_no_body, "fn init() -> int { }");

// Stmt
case!(
    stmt_while_0,
    "fn main() -> int { var x: int = 3; while(x) { x = x - 1; } return x; }"
);
case!(stmt_return_0, "fn main() -> int { return; }");

// Unary Ops
case!(unary_plus, "const y = +x;");
case!(unary_minus, "const y = -x;");
case!(unary_deref, "const y = *x;");
// case!(unary_ref, "const y = &x;");
case!(unary_atmark, "const y = @x;");
case!(unary_not, "const y = !x;");

// Binary Ops
case!(binary_add, "const z = x + y;");
case!(binary_sub, "const z = x - y;");
case!(binary_mul, "const z = x * y;");
case!(binary_div, "const z = x / y;");
case!(binary_mod, "const z = x % y;");
case!(binary_and, "const z = x & y;");
case!(binary_or, "const z = x | y;");
case!(binary_xor, "const z = x ^ y;");
case!(binary_eq, "const z = x == y;");
case!(binary_neq, "const z = x != y;");
case!(binary_lt, "const z = x < y;");
case!(binary_lte, "const z = x <= y;");
case!(binary_gt, "const z = x > y;");
case!(binary_gte, "const z = x >= y;");
case!(binary_shl, "const z = x << y;");
case!(binary_shr, "const z = x >> y;");

// Expr
case!(expr_call_0, "const c = f(1, 2, 3);");
case!(expr_nested_call, "const c = f(g(1), h(2, 3));");
case!(expr_cast_0, "const c = x : int;");
case!(expr_member_access, "const m = obj.field;");
case!(expr_member_chain, "const m = obj.a.b;");
// case!(expr_char_lit, "const c = 'A';");
case!(expr_string_lit, r#"const s = "Hello, world!";"#);
case!(expr_group_expr, "const g = (1 + 2) * (3 - 4);");

case!(
    if_0,
    "fn main() -> int { if(0) { return 1; } else { return 2; } }"
);

case!(array_lit_0, "const a = [1, 2, 3];");
case!(array_lit_1, "const a = [1, 2, 3][0];");
case!(array_lit_2, "const a = [1, 2, 3][0] + [1, 2, 3][1];");
case!(
    array_lit_3,
    "const a = [1, 2, 3][0] + [1, 2, 3][1] + [1, 2, 3][2];"
);
