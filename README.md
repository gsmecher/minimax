Minimax: a Compressed-First, Microcoded RISC-V CPU
==================================================

[![CI status](https://github.com/gsmecher/minimax/workflows/CI/badge.svg)](https://github.com/gsmecher/minimax/actions?query=workflow%3ACI)

RISC-V's compressed instruction (RVC) extension is intended as an add-on to the
regular, 32-bit instruction set, not a replacement or competitor. Its designers
designed RVC instructions to be expanded into regular 32-bit RV32I equivalents
via a pre-decoder.

What happens if we *explicitly* architect a RISC-V CPU to execute RVC
instructions, and "mop up" any RV32I instructions that aren't convenient via a
microcode layer? What architectural optimizations are unlocked as a result?

"Minimax" is an experimental RISC-V implementation intended to establish if an
RVC-optimized CPU is, in practice, any simpler than an ordinary RV32I core with
pre-decoder. While it passes a modest test suite, you should not use it without
caution. (There are a large number of excellent, open source, "little" RISC-V
implementations you should probably use reach for first.)

In short:

* RV32C (compressed) instructions are first-class and execute at 1 clock per
  instruction. (Exceptions: shifts and branches.)

* SOME RV32I instructions are directly implemented in RTL and execute in 2
  clocks per instruction.

* Other RV32I instructions are emulated in microcode, using the instructions
  above.

This is distinct from (all?) other RV32C-capable RISC-V cores, because it
really is architected for compressed first. This is not how the compressed
ISA was intended to be implemented.

A compressed-first RISC-V architecture unlocks the following:

* 1 clock per instruction (CPI) using a 2-port register file. RVC
  instructions have only 1 rd and 1 rs field. A 2-port register file
  maps cleanly into a single RAM64X1D per bit.

* A simplified 16-bit instruction path without alignment considerations. The
  processor is a modified Harvard architecture, with a separate 16-bit
  instruction bus intended to connect to a second port of the instruction
  memory.  On Xilinx, the asymmetric ports (16-bit instruction, 32-bit data)
  are reconciled using an asymmetric block RAM primitive. As a result, we don't
  have to worry about a 32-bit instruction split across two 32-bit words.

Native instructions are selected for a balance of:

* Small implementation cost. We use a 2-port register file, and 3-operand
  instructions require more ports (or more cycles and logic to share ports)

* A reasonable performance baseline. Microcode traps require many clock
  cycles, so an impoverished "direct" instruction set hurts us in two ways:

  1. by requiring more traps to microcode emulation, and
  2. by making microcode itself longer, due to an impoverished instruction
     set.

* Sufficiency. Some RV32I instructions can't be emulated using just RV32C
  and require RTL support. (For example: word/byte stores.)

We end up with the following native instructions:

* C.xxx (all RV32C, except shifts that aren't 0 or 1 bit)
* LUI, AUIPC
* ADDI/NOP, ANDI, ORI, XORI

Why is this desirable?

* Compilers (GCC, LLVM) are learning to prefer RVC instructions when
  optimizing for size. This means compiled code (with appropriate
  optimization settings) plays to Minimax's performance sweet-spot,
  preferring direct instructions to microcoded instructions.
  (see e.g. https://muxup.com/2022q3/whats-new-for-risc-v-in-llvm-15)

* RVC instructions nearly double code density, which pay for the cost of
  microcode ROM when compared against a minimalist RV32I implementation.

* It's not quite the smallest RVC implementation (SERV is smaller), but
  it is likely much faster with the appropriate compiler settings, and
  slightly less unorthodox in implementation.

What's awkward?

* RVC decoding is definitely uglier than regular RV32I. I expect this
  ugliness is better masked when RVC instructions are decoded to RV32I and
  executed as "regular" 32-bit instructions.

* The logic depth in the "execute" pipeline stage is extremely long. This CPU
  will not reach a high FMAX even on Xilinx UltraScale/UltraScale+ FPGAs.

* Store-word (SB) and store-byte (SB) instructions are not yet supported.

What's the design like?

* Three-stage pipeline (fetch, fetch2, and everything-else). The fetch
  pipeline is 2 cycles long to allow the use of embedded block RAM
  registers, which frees up more clock slack for the execution stage.
  There is a corresponding 2-cycle penalty on taken branches.

* Several "extension instructions" that use the non-standard extension space
  reserved in C.SLLI. This space allows us to add "fused" instructions
  accessible only in microcode, that perform the following:

  - "Thunk" from microcode back to standard code,
  - Move data from "user" registers into "microcode" registers and back again.

  Because these extension instructions reach deeply into the implementation
  details, they are ignored (converted to NOPs) outside emulation microcode.

Resource usage (excluding ROM and peripherals; KU060; 12-bit PC):

* Minimax: 70 FFs, 437 CLB LUTs

Compare to:

* PicoRV32: 483 FFs, 782 LUTs ("small", RV32I only)
* FemtoRV32 186 FFs, 411 LUTs ("quark", RV32I only)
* SERV: 312 FFs, 182 LUTs (no CSR or timer; RV32I only)
* PicoBlaze: 82 FFs, 103 LUTs

Minimax is competitive, even against RV32I-only cores. When comparing
against RV32IC implementations, it does better:

* SERV: 303 FFs, 336 LUTs (no CSR or timer; RV32IC)
* PicoRV32: 518 FFs, 1085 LUTs (RV32IC)

It is difficult to gather defensible benchmarks: please treat these as
approximate, and let me know if they are inaccurate.

Comments and PRs always welcome.

Graeme Smecher
gsmecher@threespeedlogic.com
