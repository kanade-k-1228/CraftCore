# TASM Language Support

VSCode extension for TASM (Typed Assembly) language syntax highlighting and language support.

## Features

- Syntax highlighting for TASM files (`.tasm`)
- Automatic bracket matching and closing
- Comment toggling support
- Code folding
- Indentation rules
- Language server integration (`tasm-lsp`):
  - Diagnostics (parse / semantic errors)
  - Go to definition (globals across modules, local vars, asm labels)
  - Hover (type signatures, const values, addresses)
  - Document symbols (outline)

## Language Server Setup

1. Build and install the server:
   ```bash
   cargo install --path tasm-lsp   # or: make i
   ```
2. Dependencies are resolved from `project.yaml` automatically. For each
   opened file the server walks up the directory tree and uses the first
   `project.yaml` it finds (the shared definition of the `proj` crate,
   also read by the `tasm` / `cemu` CLIs):
   ```yaml
   src:
     - main.tasm        # root sources (default: main.tasm)
   include:
     rtos: ../../rtos   # module name -> directory (tasm -I NAME=DIR)
   ```
   The compile unit becomes `src` + all module files under `include`.
   Each `include` key is the module root name used in references
   (`rtos::task::...`); the directory name itself does not matter.
   Opening a module file (e.g. `rtos/task.tasm` of an example project)
   analyzes it in the context of that project.
3. Fallback configuration in `.vscode/settings.json` (used only when no
   `project.yaml` is found):
   ```json
   {
     "tasm.includeDirs": ["rtos"],
     "tasm.lsp.path": "tasm-lsp"
   }
   ```
   - `tasm.includeDirs`: module include directories (`tasm -I` equivalent),
     relative to the workspace root. `name=dir` entries set the module root
     name explicitly; a bare `dir` uses its basename. `${workspaceFolder}`
     is supported in `tasm.lsp.path`.
   - Without `includeDirs`, each file is analyzed standalone.

## Development

```bash
npm install
npm run compile     # build extension.js
npm run package     # build .vsix
```

Press F5 in VS Code to launch an Extension Development Host.

## Syntax Highlighting

The extension provides highlighting for:

### Keywords
- Control flow: `if`, `else`, `while`, `return`, `break 'label`, `continue 'label`
- Declarations: `fn`, `var`, `type`, `const`, `static`, `asm`
- Types: `int`, `void`
- Operators (word): `as`, `sizeof`

### Scope Labels
- Labeled blocks: `'name: { ... }` — used as targets for `break`/`continue`

### Assembly
- Instructions: `nop`, `mov`, `add`, `sub`, `load`, `store`, `jump`, `call`, `callr`, `ret`, etc.
- Registers: `z`, `sp`, `ra`, `fp`, `a0`, `a1`, `t0`-`t3`, `s0`-`s3`
- Labels: `label_name:`

### Other
- Numbers: Decimal, hex (`0x1234`), octal (`0o777`), binary (`0b1010`)
- Strings: Double-quoted (`"..."`) and char literals (`'a'`, `'\n'`)
- Comments: `// comment`
- Operators: Arithmetic, bitwise, shift (`<<`, `>>`), comparison, logical, address-of (postfix `@`), deref (prefix `@`)
- Type names and identifiers
- Trailing commas in struct/array literals, struct/function types, and function parameter lists are accepted (e.g. `{ a: 1, b: 2, }`, `fn f(x: int, y: int,) { ... }`)

## Installation

1. Install the `.vsix` file using:
   ```bash
   code --install-extension tasm-lang-1.0.0.vsix
   ```

2. Or install from VSCode:
   - Open Command Palette (Ctrl+Shift+P / Cmd+Shift+P)
   - Run "Extensions: Install from VSIX..."
   - Select the `tasm-lang-1.0.0.vsix` file

## Usage

Open any `.tasm` file and the syntax highlighting will be automatically applied.
