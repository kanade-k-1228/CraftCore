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

b: target/release/tasm target/release/cemu
target/release/tasm:
	cargo build -p tasm --release
target/release/cemu:
	cargo build -p cemu --release

i:
	cargo install --path tasm
	cargo install --path cemu

t:
	cargo test --all

f:
	cargo fmt --all

c:
	cargo check --all
	cargo clippy --all

$(EXAMPLES): target/release/tasm target/release/cemu
	@$(MAKE) --no-print-directory -C example/$@ \
		TASM=$(CURDIR)/target/release/tasm \
		CEMU=$(CURDIR)/target/release/cemu
