TASM ?= tasm
CCEMU ?= ccemu
CCEMU_ARGS ?=

.PHONY: run clean

run: out/main.bin
	@$(CCEMU) out/main.bin out/const.bin \
		$$([ -f dump.yaml ] && echo "-d dump.yaml") \
		$$([ -f intr.yaml ] && echo "-i intr.yaml") \
		$(CCEMU_ARGS)

out/main.bin out/const.bin out/map.yaml &: main.tasm
	@mkdir -p out
	@$(TASM) main.tasm -o out/main.bin -r out/const.bin -m out/map.yaml

clean:
	rm -rf out
