EXAMPLES := $(patsubst example/%/,%,$(wildcard example/*/))

.PHONY: help b i c f t $(EXAMPLES)

help:
	@echo "Targets:"
	@echo "  make b         : Release build"
	@echo "  make i         : Install release build"
	@echo "  make c         : Check"
	@echo "  make f         : Fix"
	@echo "  make t         : Run all tests"
	@echo "  make <example> : Run example"
	@echo "    $(EXAMPLES)"

b: target/release/tasm target/release/ccemu
target/release/tasm:
	cargo build -p tasm --release
target/release/ccemu:
	cargo build -p ccemu --release

i:
	cargo install --path tasm
	cargo install --path emu

t:
	cargo test --all

f:
	cargo fmt --all

c:
	cargo check --all
	cargo clippy --all

$(EXAMPLES): target/release/tasm target/release/ccemu
	@$(MAKE) --no-print-directory -C example/$@ \
		TASM=$(CURDIR)/target/release/tasm \
		CCEMU=$(CURDIR)/target/release/ccemu
