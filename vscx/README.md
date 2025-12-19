# TASM Language Support

VSCode extension for TASM (Typed Assembly) language syntax highlighting and language support.

## Features

- Syntax highlighting for TASM files (`.tasm`)
- Automatic bracket matching and closing
- Comment toggling support
- Code folding
- Indentation rules

## Syntax Highlighting

The extension provides highlighting for:

### Keywords
- Control flow: `if`, `else`, `while`, `return`, `break`, `continue`
- Declarations: `fn`, `var`, `type`, `const`, `static`, `asm`
- Types: `int`

### Assembly
- Instructions: `nop`, `mov`, `add`, `sub`, `load`, `store`, `jump`, `call`, `ret`, etc.
- Registers: `z`, `sp`, `ra`, `fp`, `a0`, `a1`, `t0`-`t3`, `s0`-`s3`
- Labels: `label_name:`

### Other
- Numbers: Decimal and hexadecimal (`0x1234`)
- Strings: Double and single quoted
- Comments: `// comment`
- Operators: Arithmetic, bitwise, comparison, logical
- Type names and identifiers

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
