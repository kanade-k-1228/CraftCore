#!/bin/bash
cargo run --bin tasm  -q -- tasm/sample/asm.tasm -o asm.bin
cargo run --bin rkemu -q -- asm.bin -t 30
