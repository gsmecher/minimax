CROSS_CC?=riscv64-unknown-elf-gcc
CROSS_OBJCOPY?=riscv64-unknown-elf-objcopy

CFLAGS := -march=rv32ic -mabi=ilp32	\
	  -fno-pic -nostartfiles -nostdlib

LDFLAGS := -march=rv32ic -mabi=ilp32	\
	   -fno-pic -Wl,--no-relax	\
	   -nostartfiles -nostdlib

default: blink.mem

clean:
	rm -rf *.o blink.mem*

# .mem generator
blink.mem: blink.o microcode.o
	$(CROSS_CC) $(LDFLAGS) -T bare.ld -o $@.elf $^
	$(CROSS_OBJCOPY) -O binary $@.elf $@.bin
	./bin2mem $@.bin $@

# Assembler
%.o: %.S
	$(CROSS_CC) $(CFLAGS) -c -o $@ $<

# Compiler
%.o: %.c
	$(CROSS_CC) $(CFLAGS) -Os -c -o $@ $<
