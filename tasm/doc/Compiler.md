# Compiler Architecture

The `tasm` compiler takes one or more source files and produces an instruction memory image (`i.bin`), a constant ROM image (`rom.bin`), and an optional symbol map (`map.yaml`).

```
  *.tasm  ──▶ Lexer ──▶ Parser ──▶ AST ──▶ Global ──▶ deps ──▶ Allocator ──▶ binary
                                              │                                  │
                                              ▼                                  ▼
                                            code  ─────────────────────────▶  i.bin
                                            constexpr/static  ───────────▶  rom.bin
                                                                              map.yaml
```

## 1. Lex (`grammer::lexer`)

The source string is tokenized line by line:

- Single- and double-character tokens (`==`, `<=`, `->`, …) and keywords (`fn`, `asm`, `if`, `as`, `sizeof`, …)
- Number literals (`0x...` hex, decimal, `_` separators), char literals (`'A'`, `'\n'`), and string literals (`"..."`)
- `//` line comments are skipped
- Every token carries a `Pos` (file, row, column) for diagnostics

## 2. Parse (`grammer::parser`)

Tokens are turned into an AST. The top-level `AST` is a `Vec<Def>` where each `Def` is one of:

| Def    | Form                                                      |
| ------ | --------------------------------------------------------- |
| Type   | `type ident = type;`                                      |
| Const  | `const [@ expr] ident = expr;` (initializer required)     |
| Static | `static [@ expr] ident : type;`                           |
| Asm    | `asm [@ expr] ident { asm_stmt* }`                        |
| Func   | `fn ident(args) [-> type] { stmt* }`                      |

`Stmt`, `Expr`, and `Type` are recursive enums. Expressions are parsed by recursive descent following the precedence chain `or → xor → and → eq → relat → shift → add → mul → unary → postfix → prim`.

On a parse error the parser records the error and skips ahead to the next top-level keyword (`type` / `const` / `static` / `asm` / `fn`), so multiple errors can be reported in one run.

## 3. Global (`eval::global`)

`Global` is a name-keyed `IndexMap<&str, &Def>` over the AST. Duplicate definitions are caught here.

`Global` exposes **lazy, cached** evaluation primitives (`RwLock<HashMap<...>>`):

| API                | Role                                                                                  |
| ------------------ | ------------------------------------------------------------------------------------- |
| `normtype(ty)`     | Resolve `Type::Custom` and array lengths into a `NormType` (primitive-only canonical) |
| `constexpr(expr)`  | Evaluate a constant expression to a `ConstExpr` (Number/Char/String/Array/Struct)     |
| `typeinfer(expr)`  | Infer the `NormType` of an expression                                                 |
| `addrexpr(expr)`   | Resolve `ident[i].field` into `(symbol, offset)`                                      |
| `code(name)`       | Generate code (`Vec<Inst<Reg, Imm>>`) for an `asm` or `fn` definition                 |

The `get_*_resolved(name)` helpers package pre-link information (fixed `@` address, size, etc.) for a given symbol.

### NormType

`NormType` is the canonical form with all `Type::Custom` resolved and array lengths reduced to `usize`. `NormType::sizeof()` computes the storage size in 16-bit words: `int = 1`, `void = 0`, `*T = 1`, `[N]T = N * <T>`, `struct = Σ <fields>`, `func = 0`.

### ConstExpr

`ConstExpr` is the evaluated form of a constant expression (`Number`, `Char`, `String`, `Array`, `Struct`). It is reused for const initializers, array lengths, and fixed-address expressions.

## 4. Code Generation

`Global::code(name)` dispatches to one of the two backends below.

### 4-1. `asm2code` (`eval::asm`)

Each statement of an `asm` block is translated to an `arch::inst::Inst<Reg, Imm>`:

- Arguments are interpreted as register (`reg`), immediate (`imm`), global label (`global`), or local label (`local`)
- `Imm` is an unresolved immediate with four variants: `Lit` (literal value), `Const` (named constant), `Symbol` (data symbol + offset), `Label` (code label)
- Local labels are resolved immediately to PC-relative offsets within the same `asm` block

### 4-2. `func2code` (`eval::func`)

A `fn` is lowered to an assembly sequence with a full stack frame:

- `Local` (`eval::local`) assigns stack offsets — arguments at positive offsets from `FP`, locals at negative offsets
- The prologue reserves stack space, saves `RA` and `FP`, sets the new `FP`, and stores incoming args (`A0`, `A1`) to their stack slots
- Statements are lowered recursively via `compile_stmt` / `compile_expr` / `compile_lvalue`; `if` / `while` expand to PC-relative `JUMPIFR` / `JUMPR` instructions
- The epilogue restores `FP`, `RA`, deallocates the frame, and emits `RET`

Calling convention: the first two arguments go in `A0` / `A1`, the rest on the stack. The return value is returned in `A0`.

## 5. Dependency Resolution (`eval::deps`)

`Global::deps(entries, ...)` walks the call graph from a fixed set of entry points (by default `reset`, `irq`, `main`) and collects:

- Code labels reachable through `Imm::Label(name)` (added to `labels`)
- Data symbols reachable through `Imm::Symbol(name, _)` (added to `symbols`)

Unreferenced definitions are excluded from linking.

## 6. Allocator (`linker::allocator`, `linker::memory`)

`Memory::new(begin, end).section(name, start, end)...` declares an address space with named sections; `Allocator::from_memory(...)` then places objects.

### 6-1. Instruction memory

```
0x0000..0x0004  reset    section
0x0004..0x0008  irq      section
0x0008..0x10000 code     section
```

- Objects with a fixed address (`asm @ ...`) are placed with `allocate(addr, size, name)`
- Objects without a fixed address are packed into the `code` section via `section("code", size, name)`

### 6-2. Data memory

```
0x3000..0x5000   const    section
0x5000..0x10000  static   section
```

- Fixed-address `const` / `static` definitions go through `allocate`
- Otherwise `const` definitions fall into the `const` section and `static` definitions into the `static` section

Internally `Allocator` maintains a sorted list of `Segment`s spanning the address space. Each `allocate` call splits the matching segment into up to three (leading free / occupied / trailing free). Overlapping or out-of-range allocations are reported as errors.

## 7. Binary Generation (`linker::binary`)

Using the finalized `imap` / `dmap` (symbol → address):

- `genibin` resolves each instruction's `Imm` (`Label` → code address, `Symbol` → data address + offset, `Const` → value, `Lit` → as-is), encodes via `Inst::to_op().to_bin()`, and writes the 32-bit instructions in little-endian into the instruction memory image
- `gencbin` serializes each `const`'s `ConstExpr::bin()` to its allocated address to produce the data ROM image

## 8. Symbol Map (`util::maps`)

`SymbolMap::generate` emits the code and data placement as YAML (`map.yaml`):

```yaml
code:
  main:
    addr: 0x...
    size: 0x...
    stacks: { ... }
data:
  serial_tx:
    addr: 0x...
    size: 0x...
```

This is consumed by external debuggers / monitors.

## main.rs flow

1. Read each `*.tasm` and tokenize with `Lexer`
2. Merge all tokens and parse into a single `AST` with `Parser`
3. Build `Global::new(ast)`
4. Collect reachable symbols via `deps(&["reset", "irq", "main"])`
5. Build the instruction-memory `Allocator`; place fixed-address `asm` then auto-allocate the rest into `code`
6. Build the data-memory `Allocator`; place fixed-address `const` / `static` then auto-allocate the rest into `const` / `static`
7. Emit binaries via `genibin` / `gencbin`
8. Write outputs (`-o` / `-r` / `-m`)
