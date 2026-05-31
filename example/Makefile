TASM ?= tasm
CEMU ?= cemu
SRCS ?= main.tasm
ARGS ?=

.PHONY: run clean

run: out/main.bin
	@$(CEMU) out/main.bin out/const.bin \
		$$([ -f dump.yaml ] && echo "-d dump.yaml") \
		$$([ -f intr.yaml ] && echo "-i intr.yaml") \
		$(ARGS)

out/main.bin out/const.bin out/map.yaml &: $(SRCS)
	@mkdir -p out
	@$(TASM) $(SRCS) -o out/main.bin -r out/const.bin -m out/map.yaml

clean:
	rm -rf out
