.PHONY: build release test fmt check install clean help

help:
	@echo "Targets:"
	@echo "  build     - cargo build --all"
	@echo "  release   - cargo build --all --release"
	@echo "  test      - cargo test --all"
	@echo "  fmt       - cargo fmt --all"
	@echo "  check     - cargo check && cargo clippy"
	@echo "  install   - install tasm and rkemu"
	@echo "  clean     - cargo clean"

build:
	cargo build --all

release:
	cargo build --all --release

test:
	cargo test --all

fmt:
	cargo fmt --all

check:
	cargo check --all
	cargo clippy --all

install:
	cargo install --path tasm
	cargo install --path emu

clean:
	cargo clean

.DEFAULT_GOAL := help
