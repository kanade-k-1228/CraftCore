EXAMPLES := $(patsubst example/%/,%,$(wildcard example/*/))

.PHONY: help b i c f t $(EXAMPLES)

help:
	@echo "Targets:"
	@echo "  make b  : Release build"
	@echo "  make i  : Install release build"
	@echo "  make c  : Check"
	@echo "  make f  : Fix"
	@echo "  make t  : Run all tests"
	@echo "  make *  : Run example"
	@echo "    $(EXAMPLES)"

b: target/release/tasm target/release/cemu target/release/tasm-lsp
target/release/tasm:
	cargo build -p tasm --release
target/release/cemu:
	cargo build -p cemu --release
target/release/tasm-lsp:
	cargo build -p tasm-lsp --release

i:
	cargo install --path tasm
	cargo install --path cemu
	cargo install --path tasm-lsp

t:
	cargo test --all

f:
	cargo fmt --all

c:
	cargo check --all
	cargo clippy --all

$(EXAMPLES): target/release/tasm target/release/cemu
	@cd example/$@ && $(CURDIR)/target/release/tasm && $(CURDIR)/target/release/cemu
