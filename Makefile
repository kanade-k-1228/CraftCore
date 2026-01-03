# CraftCore Makefile
# Commands for TASM compiler and RK16 emulator

# Default target
.PHONY: all
all: build

# Build commands
.PHONY: build
build:
	@echo "Building all components..."
	cargo build --all

.PHONY: build-tasm
build-tasm:
	@echo "Building TASM compiler..."
	cargo build --package tasm

.PHONY: build-emu
build-emu:
	@echo "Building RK16 emulator..."
	cargo build --package rkemu

.PHONY: build-arch
build-arch:
	@echo "Building architecture library..."
	cargo build --package arch

# Release builds
.PHONY: release
release:
	@echo "Building all components in release mode..."
	cargo build --all --release

# Test commands
.PHONY: test
test:
	@echo "Running all tests..."
	cargo test --all

.PHONY: test-tasm
test-tasm:
	@echo "Running TASM tests..."
	cargo test --package tasm

# Compile TASM files
.PHONY: compile
compile:
	@echo "Usage: make compile FILE=<input.tasm> [OUT=<output_dir>] [MAP=1] [VERBOSE=1]"
	@echo "Example: make compile FILE=sample/00_asm_test/main.tasm MAP=1"

# Actual compile target with file checking
ifdef FILE
compile: $(FILE)
	@echo "Compiling $(FILE)..."
	@if [ -z "$(OUT)" ]; then \
		OUT_DIR=$$(dirname $(FILE))/out; \
	else \
		OUT_DIR=$(OUT); \
	fi; \
	FLAGS=""; \
	if [ "$(MAP)" = "1" ]; then \
		FLAGS="$$FLAGS -m"; \
	fi; \
	if [ "$(VERBOSE)" = "1" ]; then \
		FLAGS="$$FLAGS -v"; \
	fi; \
	cargo run --bin tasm -- $(FILE) -o $$OUT_DIR $$FLAGS
	@echo -n "Output files: $$OUT_DIR/main.bin, $$OUT_DIR/const.bin"
	@if [ "$(MAP)" = "1" ]; then \
		echo ", $$OUT_DIR/map.yaml"; \
	else \
		echo ""; \
	fi
endif

# Run emulator with new file names
.PHONY: run
run:
	@echo "Usage: make run MAIN=<main.bin> [CONST=<const.bin>] [TMAX=<steps>]"
	@echo "Example: make run MAIN=sample/00_asm_test/out/main.bin"

ifdef MAIN
run:
	@echo "Running emulator with $(MAIN)..."
	@if [ -z "$(CONST)" ]; then \
		CONST_PATH=$$(echo $(MAIN) | sed 's/main\.bin/const.bin/'); \
	else \
		CONST_PATH=$(CONST); \
	fi; \
	if [ -z "$(TMAX)" ]; then \
		cargo run --bin rkemu -- $(MAIN) $$CONST_PATH; \
	else \
		cargo run --bin rkemu -- $(MAIN) $$CONST_PATH -t $(TMAX); \
	fi
endif

# Compile and run in one command
.PHONY: compile-run
compile-run:
	@echo "Usage: make compile-run FILE=<input.tasm> [TMAX=<steps>]"
	@echo "Example: make compile-run FILE=sample/00_asm_test/main.tasm TMAX=50"

ifdef FILE
compile-run: $(FILE)
	@echo "Compiling and running $(FILE)..."
	@OUT_DIR=$$(dirname $(FILE))/out; \
	cargo run --bin tasm -- $(FILE) -o $$OUT_DIR; \
	if [ -z "$(TMAX)" ]; then \
		cargo run --bin rkemu -- $$OUT_DIR/main.bin $$OUT_DIR/const.bin; \
	else \
		cargo run --bin rkemu -- $$OUT_DIR/main.bin $$OUT_DIR/const.bin -t $(TMAX); \
	fi
endif

# Sample programs shortcuts
.PHONY: sample-asm
sample-asm:
	@echo "Compiling and running assembly test..."
	@make compile-run FILE=sample/00_asm_test/main.tasm TMAX=50

.PHONY: sample-func
sample-func:
	@echo "Compiling and running function test..."
	@make compile-run FILE=sample/01_func_test/main.tasm TMAX=100

.PHONY: sample-fib
sample-fib:
	@echo "Compiling and running Fibonacci..."
	@make compile-run FILE=sample/02_fib/main.tasm TMAX=1000

.PHONY: sample-serial
sample-serial:
	@echo "Compiling and running serial test..."
	@make compile-run FILE=sample/03_serial_test/main.tasm TMAX=100

# Test seq blocks
.PHONY: test-seq
test-seq:
	@echo "Testing seq blocks..."
	cargo run --bin tasm -- test_seq_used.tasm -o test_out -v
	@echo "Output: test_out/main.bin, test_out/const.bin, test_out/map.yaml"

# Clean commands
.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	cargo clean
	find . -type d -name "out" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name "new_out" -exec rm -rf {} + 2>/dev/null || true
	rm -rf test_out test_out2 2>/dev/null || true

.PHONY: clean-out
clean-out:
	@echo "Cleaning output directories..."
	find . -type d -name "out" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name "new_out" -exec rm -rf {} + 2>/dev/null || true
	rm -rf test_out test_out2 2>/dev/null || true

# Format code
.PHONY: fmt
fmt:
	@echo "Formatting code..."
	cargo fmt --all

# Check code
.PHONY: check
check:
	@echo "Checking code..."
	cargo check --all
	cargo clippy --all

# Help
.PHONY: help
help:
	@echo "CraftCore Makefile Commands:"
	@echo ""
	@echo "Build Commands:"
	@echo "  make build          - Build all components"
	@echo "  make build-tasm     - Build TASM compiler only"
	@echo "  make build-emu      - Build RK16 emulator only"
	@echo "  make release        - Build all in release mode"
	@echo ""
	@echo "Test Commands:"
	@echo "  make test           - Run all tests"
	@echo "  make test-tasm      - Run TASM tests only"
	@echo "  make test-seq       - Test seq block compilation"
	@echo ""
	@echo "Compile & Run Commands:"
	@echo "  make compile FILE=<input.tasm> [OUT=<dir>] [MAP=1] [VERBOSE=1]"
	@echo "                      - Compile a TASM file"
	@echo "                      - Outputs: main.bin, const.bin, (map.yaml if MAP=1)"
	@echo "  make run MAIN=<main.bin> [CONST=<const.bin>] [TMAX=<steps>]"
	@echo "                      - Run emulator with binary files"
	@echo "  make compile-run FILE=<input.tasm> [TMAX=<steps>]"
	@echo "                      - Compile and run in one command"
	@echo ""
	@echo "Sample Programs:"
	@echo "  make sample-asm     - Run assembly test"
	@echo "  make sample-func    - Run function test"
	@echo "  make sample-fib     - Run Fibonacci test"
	@echo "  make sample-serial  - Run serial test"
	@echo ""
	@echo "Maintenance Commands:"
	@echo "  make clean          - Clean all build artifacts"
	@echo "  make clean-out      - Clean output directories only"
	@echo "  make fmt            - Format code with rustfmt"
	@echo "  make check          - Check code with cargo check and clippy"
	@echo ""
	@echo "Output Files:"
	@echo "  main.bin   - Main program binary"
	@echo "  const.bin  - Constant data binary"
	@echo "  map.yaml   - Combined symbol map (fn_map, static_map, data_map)"
	@echo ""
	@echo "Examples:"
	@echo "  make compile FILE=main.tasm"
	@echo "  make compile FILE=main.tasm OUT=build VERBOSE=1"
	@echo "  make run MAIN=out/main.bin CONST=out/const.bin TMAX=100"
	@echo "  make compile-run FILE=sample/00_asm_test/main.tasm"

# Default target shows help
.DEFAULT_GOAL := help