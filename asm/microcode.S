/*
 * microcode.s: RV32I emulation for the Minimax C.x processor core.
 *
 * (c) 2022 Three-Speed Logic, Inc. All rights reserved.
 */

.macro x.poke rd, rs
	c.slli \rd, 0x20
	c.mv x8, \rs
.endm
.macro x.peek rd, rs
	c.slli \rs, 0x21
	c.mv \rd, x8
.endm
.macro x.thunk rd
	c.slli \rd, 0x22
.endm

/* Trapping looks like a JAL with a microcode-specific register bank.
 * At the point of entry,
 *
 * - Register 0x21 (that's "microcode" x1) contains the return address we should jump to
 *   (that's the trap PC, plus 2).
 *
 * It is not safe to use emulated instructions here, since the CPU will
 * double-trap. Instead, use jal to call the emulated instruction locally (if
 * we can stick to some sort of ABI)
 *
 * Because C.x instructions have such a limited range, we use the following ABI:
 *
 * x1 / ra: reserved for 1-deep function calls/returns
 * x2 / sp: RESERVED - could be pointer to microcode constants/variables
 * x3: offending PC
 * x4: offending instruction
 * x5: opcode
 * x6: rd field
 * x7: opcode bits 31..12, right shifted
 *
 * x8..15: working registers
 *
 * x16: funct3, left shifted by 1
 * x17: rs1 field
 * x18: rs1 value
 * x19: rs2/shamt field
 * x20: rs2 value
 *
 * All other (microcode) registers are currently unused.
 */

.section .mctext

microcode_entry:
	/* Capture offending PC. Because we capture the fetch PC (not execute
	 * PC) at time of trap, it's been auto-incremented and needs to be
	 * decremented to match the executing PC. */
	addi x3, ra, -2

	/* Determine if the opcode is aligned or not. 32-bit RV32IC instructions
	 * can straddle 32-bit boundaries. */
	c.mv x8, x3
	c.andi x8, 3
	c.beqz x8, aligned

	/* Misaligned opcode: stitch together from adjacent 32-bit accesses. */
	addi x8, x3, -2
	c.lw x9, 0(x8)
	c.lw x8, 4(x8)
	c.srli x9, 16
	c.slli x8, 16
	c.or x8, x9
	c.mv x4, x8
	c.j 1f

aligned:
	/* Aligned instruction: fetch into x8; copy into x4 */
	c.mv x8, x3
	c.lw x8, 0(x8)
	c.mv x4, x8

1:	/* Isolate opcode into x5 - note we strip the lower bits, which are always 11 */
	c.srli x8, 2
	andi x5, x8, 0x1f

	/* Isolate rd */
	c.srli x8, 5
	andi x6, x8, 0x1f

	/* isolate funct3, left shifted by 1 for jump tables */
	c.srli x8, 4
	andi x16, x8, 0xe
	c.srli x8, 1

	/* isolate rs1 */
	c.srli x8, 3
	andi x17, x8, 0x1f

	/* look up rs1 value from register file (we mostly need it) */
	x.peek x18, x17

	/* isolate rs2/shamt */
	c.srli x8, 5
	andi x19, x8, 0x1f

	/* look up rs2 value from register file (we sometimes need it) */
	x.peek x20, x19

	/* create jump based on opcode */
	c.mv x8, x5
	c.slli x8, 1 /* 1 compressed instruction per opcode */

	la x9, table_opcode
	c.add x8, x9
	c.jr x8

fail:	c.j fail

	/* FIXME: this should not be necessary */
	c.nop

table_opcode:
	c.j table0	/* 0 */
	c.j fail	/* 1 */
	c.j fail	/* 2 */
	c.j fail	/* 3 */
	c.j table4	/* 4 */
	c.j fail	/* 5 */
	c.j fail	/* 6 */
	c.j fail	/* 7 */
	c.j table8	/* 8 */
	c.j fail	/* 9 */
	c.j fail	/* a */
	c.j fail	/* b */
	c.j tablec	/* c */
	c.j fail	/* d */
	c.j fail	/* e */
	c.j fail	/* f */
	c.j fail	/* 10 */
	c.j fail	/* 11 */
	c.j fail	/* 12 */
	c.j fail	/* 13 */
	c.j fail	/* 14 */
	c.j fail	/* 15 */
	c.j fail	/* 16 */
	c.j fail	/* 17 */
	c.j table18	/* 18 */
	c.j jalr	/* 19 */
	c.j fail	/* 1a */
	c.j jal		/* 1b */
	c.j fail	/* 1c */
	c.j fail	/* 1d */
	c.j fail	/* 1e */
	c.j fail	/* 1f */

table0:
	la x9, 1f
	c.add x9, x16
	c.jr x9

1:	c.j lb /* 0.0: LB */
	c.j lh /* 0.1: LH */
	c.j lw /* 0.2: LW */
	c.j fail /* 0.3: FENCE */
	c.j lbu /* 0.4: LBU */
	c.j lhu /* 0.5: LHU*/
	c.j fail /* 0.6: */
	c.j fail /* 0.7: */

table4:
	la x9, 1f
	c.add x9, x16
	c.jr x9

1:	c.j fail /* 4.0: ADDI - implemented in RTL */
	c.j fail /* 4.1: SLLI - implemented in RTL */
	c.j slti /* 4.2: SLTI */
	c.j sltiu /* 4.3: SLTIU */
	c.j fail /* 4.4: XORI - implemented in RTL */
	c.j fail /* 4.5: SRLI/SRAI - implemented in RTL */
	c.j fail /* 4.6: ORI - implemented in RTL */
	c.j fail /* 4.7: ANDI - implemented in RTL */

table8:
	la x9, 1f
	c.add x9, x16
	c.jr x9

1:	c.j fail /* 8.0: SB */
	c.j fail /* 8.1: SH */
	c.j sw /* 8.2: SW */
	c.j fail /* 8.3: */
	c.j fail /* 8.4: */
	c.j fail /* 8.5: */
	c.j fail /* 8.6: */
	c.j fail /* 8.7: */

tablec:
	la x9, 1f
	c.add x9, x16
	c.jr x9

1:	c.j add_sub /* c.0: ADD/SUB */
	c.j fail /* c.1: SLL - implemented in RTL */
	c.j slt /* c.2: SLT */
	c.j fail /* c.3: SLTU */
	c.j xor /* c.4: XOR */
	c.j fail /* c.5: SRL/SRA - implemented in RTL */
	c.j or /* c.6: OR */
	c.j and /* c.7: AND */

table18:
	la x9, 1f
	c.add x9, x16
	c.jr x9

1:	c.j beq /* 18.0: BEQ */
	c.j bne /* 18.1: BNE */
	c.j fail /* 18.2: */
	c.j fail /* 18.3: */
	c.j blt /* 18.4: BLT */
	c.j bge /* 18.5: BGE */
	c.j bltu /* 18.6: BLTU */
	c.j bgeu /* 18.7: BGEU */

/*
 * FIXME: loads do not gracefully handle misaligned addresses.
 */

lb:	c.jal load_form_address
	c.lw x8, 0(x8)

	c.addi x9, -3
1:	c.beqz x9, 3f
2:	c.slli x8, 8
	c.addi x9, 1
	c.bnez x9, 2b

3:	c.srai x8, 24
	x.poke x6, x8
	c.j ret_rv32

lh:	c.jal load_form_address
	c.lw x8, 0(x8)
	c.bnez x9, 1f
	c.slli x8, 16
1:	c.srai x8, 16
	x.poke x6, x8
	c.j ret_rv32

lw:	c.jal load_form_address
	c.lw x8, 0(x8)
	x.poke x6, x8
	c.j ret_rv32

lbu:	c.jal load_form_address
	c.lw x8, 0(x8)

	c.addi x9, -3
1:	c.beqz x9, 3f
2:	c.slli x8, 8
	c.addi x9, 1
	c.bnez x9, 2b

3:	c.srli x8, 24
	x.poke x6, x8
	c.j ret_rv32


lhu:	c.jal load_form_address
	c.lw x8, 0(x8)
	c.bnez x9, 1f
	c.slli x8, 16
1:	c.srli x8, 16
	x.poke x6, x8
	c.j ret_rv32

load_form_address:
	# x8 -> 32-bit address, possibly unaligned
	c.mv x8, x4
	c.srai x8, 20
	c.add x8, x18

	# x8 -> 32-bit address; x9 -> address LSBs
	andi x9, x8, 3
	c.andi x8, -4

	c.jr ra

sw:	c.mv x8, x4
	c.srai x8, 20
	c.andi x8, -32 # drop bits 24..20 - these encode rs2
	c.add x8, x6 # low offset bits
	c.add x8, x18 # base address
	c.mv x9, x20
	c.sw x9, 0(x8)
	c.j ret_rv32

/* Placed here because c.bnez/c.beqz have limited range and are used in
 * relative branches */
ret_rv32:
	c.addi x3, 4
	x.thunk x3

beq:
	c.mv x8, x18
	c.mv x9, x20
	c.sub x8, x9
	c.bnez x8, ret_rv32 /* branch not taken */

	c.jal resolve_imm1
	c.add x8, x3
	x.thunk x8

bne:
	c.mv x8, x18
	c.mv x9, x20
	c.sub x8, x9
	c.beqz x8, ret_rv32 /* branch not taken */

	c.jal resolve_imm1
	c.add x8, x3
	x.thunk x8

blt:
	c.mv x8, x18
	c.mv x9, x20
	c.jal slt_func
	c.beqz x8, ret_rv32 /* branch not taken */

	c.jal resolve_imm1
	c.add x8, x3
	x.thunk x8

slt:
	c.mv x8, x18
	c.mv x9, x20
	c.jal slt_func
	x.poke x6, x8
	c.j ret_rv32

slti:
	c.mv x8, x18
	c.mv x9, x4
	c.srai x9, 20
	c.jal slt_func
	x.poke x6, x8
	c.j ret_rv32

sltiu:
	c.mv x8, x18
	c.mv x9, x4
	c.srai x9, 20
	c.jal sltu_func
	x.poke x6, x8
	c.j ret_rv32

bge:
	c.mv x8, x18
	c.mv x9, x20
	c.sub x8, x9

	lui x9, 0x80000
	c.and x8, x9
	c.bnez x8, ret_rv32 /* branch not taken */

	c.jal resolve_imm1
	c.add x8, x3
	x.thunk x8

bltu:
	c.mv x8, x18
	c.mv x9, x20

	c.jal sltu_func
	c.beqz x8, ret_rv32

	/* take the branch */
	c.jal resolve_imm1
	c.add x8, x3
	x.thunk x8

slt_func:
	/* Compare MSBs */
	c.mv x10, x8
	c.xor x10, x9
	c.srli x10, 31
	c.beqz x10, 1f

	/* MSBs differed: right-shift to avoid overflow */
	c.srai x8, 1
	c.srai x9, 1

1:	/* MSBs were the same. Compare directly. */
	c.sub x8, x9
	c.srli x8, 31

	c.jr ra

sltu_func:
	/* Compare MSBs */
	c.mv x10, x8
	c.xor x10, x9
	c.srli x10, 31
	c.beqz x10, 1f

	/* MSBs differed: right-shift to avoid overflow */
	c.srli x8, 1
	c.srli x9, 1

1:	/* MSBs were the same. Compare directly. */
	c.sub x8, x9
	c.srli x8, 31

	c.jr ra

bgeu:
	c.mv x8, x18
	c.mv x9, x20

	c.jal sltu_func
	c.bnez x8, ret_rv32

	/* take the branch */
	c.jal resolve_imm1
	c.add x8, x3
	x.thunk x8

add_sub:
	c.mv x8, x18
	c.mv x9, x20

	/* disambiguate add/sub */
	c.mv x10, x4
	lui x11, 0x40000
	c.and x10, x11
	c.beqz x10, 1f
	c.li x10, -1
	c.xor x9, x10
	c.addi x9, 1

1:	c.add x8, x9
	x.poke x6, x8
	c.j ret_rv32

xor:
	c.mv x8, x18
	c.mv x9, x20
	c.xor x8, x9
	x.poke x6, x8
	c.j ret_rv32

or:
	c.mv x8, x18
	c.mv x9, x20
	c.or x8, x9
	x.poke x6, x8
	c.j ret_rv32

and:
	c.mv x8, x18
	c.mv x9, x20
	c.and x8, x9
	x.poke x6, x8
	c.j ret_rv32

jalr:
	/* Save pc+4 to rd */
	c.mv x9, x3
	c.addi x9, 4
	x.poke x6, x9

	/* Resolve immediate and add to rd */
	c.mv x8, x4
	c.srai x8, 20
	c.add x8, x18
	c.andi x8, -2 /* zero LSB */

	/* Thunk there */
	x.thunk x8

jal:
	/* sign extend into imm[20] */
	c.mv x8, x4
	lui x9, 0x80000
	c.and x8, x9
	c.srai x9, 11

	/* imm[19:12] */
	c.mv x9, x4
	c.slli x9, 12 /* clear upper bits */
	c.srli x9, 24 /* clear lower bits */
	c.slli x9, 12 /* move into place */
	c.or x8, x9

	/* imm[11] */
	andi x9, x19, 1
	c.slli x9, 11
	c.or x8, x9

	/* imm[10:1] */
	c.mv x9, x4
	c.slli x9, 1
	c.srli x9, 21
	c.andi x9, -2
	c.or x8, x9

	/* Write return address into rd */
	c.mv x9, x3
	c.addi x9, 4
	x.poke x6, x9

	/* Form pc-relative offset and thunk there */
	c.add x8, x3
	x.thunk x8

resolve_imm1:
	/* Signed immediate per BEQ and friends into x8; x9 destroyed */
	c.mv x8, x4
	lui x9, 0x80000
	c.and x8, x9
	c.srai x8, (31-12) /* sign extend into imm[12] */

	/* pick imm[11] */
	andi x9, x6, 1
	c.slli x9, 11
	c.or x8, x9

	/* pick imm[10:5] */
	c.mv x9, x4
	lui x10, 0x7e000
	c.and x9, x10
	c.srli x9, 20
	c.or x8, x9

	/* pick imm[4:1] */
	andi x9, x6, 0x1e /* mask LSB */
	c.or x8, x9
	c.jr ra

	/* probably not needed */
	c.nop
