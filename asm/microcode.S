/*
 * microcode.s: RV32I emulation for the Minimax C.x processor core.
 *
 * (c) 2022 Three-Speed Logic, Inc. All rights reserved.
 */

.macro x.poke rd, rs
	.half 0x1006 + (\rd << 7)
	c.mv x8, \rs
.endm
.macro x.peek rd, rs
	.half 0x100a + (\rs << 7)
	c.mv \rd, x8
.endm
.macro x.thunk rd
	.half 0x1012 + (\rd << 7)
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
	/* Trapping stores PC+2 in RA. Correct it. */
	c.mv x3, ra
	c.addi x3, -2

	/* Fetch instruction, which may be half-word aligned. */
	c.mv x15, x3
	c.andi x15, 3
	c.mv x8, x3
	c.andi x8, -4	/* strip LSBs and fetch */
	c.lw x9, 0(x8)
	c.mv x4, x9
	c.beqz x15, 1f /* If aligned - that's all we needed. */

	/* Half-aligned - fetch the other half */
	c.srli x9, 16
	c.lw x10, 4(x8)
	c.slli x10, 16
	c.or x9, x10
	c.mv x4, x9

1:	/* Isolate opcode into x5 - note we strip the lower bits, which are always 11 */
	c.srli x9, 2
	c.mv x8, x9
	c.andi x9, 0x1f
	c.mv x5, x9

	/* Isolate rd */
	c.srli x8, 5
	c.mv x9, x8
	c.andi x9, 0x1f
	c.mv x6, x9

	/* isolate funct3, left shifted by 1 for jump tables */
	c.srli x8, 4
	c.mv x9, x8
	c.andi x9, 0xe
	c.mv x16, x9
	c.srli x8, 1

	/* isolate rs1 */
	c.srli x8, 3
	c.mv x9, x8
	c.andi x9, 0x1f
	c.mv x17, x9

	/* look up rs1 value from register file (we mostly need it) */
	x.peek x18, 17

	/* isolate rs2/shamt */
	c.srli x8, 5
	c.mv x9, x8
	c.andi x9, 0x1f
	c.mv x19, x9

	/* look up rs2 value from register file (we sometimes need it) */
	x.peek x20, 19

	/* create jump based on opcode */
	c.mv x8, x5
	c.slli x8, 1 /* 1 compressed instruction per opcode */

	/* Table jump */
	c.jal .+2
	c.addi ra, 6 /* offset to table base */
	c.add ra, x8
	c.jr ra

	c.j table0	/* 0 */
	c.j .		/* 1 */
	c.j .		/* 2 */
	c.j .		/* 3 */
	c.j table4	/* 4 */
	c.j auipc	/* 5 */
	c.j .		/* 6 */
	c.j .		/* 7 */
	c.j table8	/* 8 */
	c.j .		/* 9 */
	c.j .		/* a */
	c.j .		/* b */
	c.j tablec	/* c */
	c.j lui		/* d */
	c.j .		/* e */
	c.j .		/* f */
	c.j .		/* 10 */
	c.j .		/* 11 */
	c.j .		/* 12 */
	c.j .		/* 13 */
	c.j .		/* 14 */
	c.j .		/* 15 */
	c.j .		/* 16 */
	c.j .		/* 17 */
	c.j table18	/* 18 */
	c.j jalr	/* 19 */
	c.j .		/* 1a */
	c.j jal		/* 1b */
	c.j .		/* 1c */
	c.j .		/* 1d */
	c.j .		/* 1e */
	c.j .		/* 1f */

table0:
	c.jal .+2
	c.addi ra, 6 /* offset to table base */
	c.add ra, x16
	c.jr ra

	c.j lb	/* 0.0: LB */
	c.j lh	/* 0.1: LH */
	c.j lw	/* 0.2: LW */
	c.j .	/* 0.3: FENCE */
	c.j lbu	/* 0.4: LBU */
	c.j lhu	/* 0.5: LHU*/
	c.j .	/* 0.6: */
	c.j .	/* 0.7: */

table4:
	c.jal .+2
	c.addi ra, 6 /* offset to table base */
	c.add ra, x16
	c.jr ra

	c.j addi	/* 4.0: ADDI */
	c.j slli	/* 4.1: SLLI */
	c.j slti	/* 4.2: SLTI */
	c.j sltiu	/* 4.3: SLTIU */
	c.j xori	/* 4.4: XORI */
	c.j srli_srai	/* 4.5: SRLI/SRAI */
	c.j ori		/* 4.6: ORI */
	c.j andi	/* 4.7: ANDI */

table8:
	c.jal .+2
	c.addi ra, 6 /* offset to table base */
	c.add ra, x16
	c.jr ra

	c.j .	/* 8.0: SB */
	c.j .	/* 8.1: SH */
	c.j sw	/* 8.2: SW */
	c.j .	/* 8.3: */
	c.j .	/* 8.4: */
	c.j .	/* 8.5: */
	c.j .	/* 8.6: */
	c.j .	/* 8.7: */

tablec:
	c.jal .+2
	c.addi ra, 6 /* offset to table base */
	c.add ra, x16
	c.jr ra

	c.j add_sub	/* c.0: ADD/SUB */
	c.j sll		/* c.1: SLL */
	c.j slt		/* c.2: SLT */
	c.j .		/* c.3: SLTU */
	c.j xor		/* c.4: XOR */
	c.j srl_sra	/* c.5: SRL/SRA */
	c.j or		/* c.6: OR */
	c.j and		/* c.7: AND */

table18:
	c.jal .+2
	c.addi ra, 6 /* offset to table base */
	c.add ra, x16
	c.jr ra

	c.j beq		/* 18.0: BEQ */
	c.j bne		/* 18.1: BNE */
	c.j .		/* 18.2: */
	c.j .		/* 18.3: */
	c.j blt		/* 18.4: BLT */
	c.j bge		/* 18.5: BGE */
	c.j bltu	/* 18.6: BLTU */
	c.j bgeu	/* 18.7: BGEU */

lui:	c.mv x8, x4
	c.srli x8, 12
	c.slli x8, 12
	x.poke 6, x8
	c.j ret_rv32

auipc:	c.mv x8, x4
	c.srli x8, 12
	c.slli x8, 12
	c.add x8, x3
	x.poke 6, x8
	c.j ret_rv32

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
	x.poke 6, x8
	c.j ret_rv32

lh:	c.jal load_form_address
	c.lw x8, 0(x8)
	c.bnez x9, 1f
	c.slli x8, 16
1:	c.srai x8, 16
	x.poke 6, x8
	c.j ret_rv32

lw:	c.jal load_form_address
	c.lw x8, 0(x8)
	x.poke 6, x8
	c.j ret_rv32

lbu:	c.jal load_form_address
	c.lw x8, 0(x8)

	c.addi x9, -3
1:	c.beqz x9, 3f
2:	c.slli x8, 8
	c.addi x9, 1
	c.bnez x9, 2b

3:	c.srli x8, 24
	x.poke 6, x8
	c.j ret_rv32


lhu:	c.jal load_form_address
	c.lw x8, 0(x8)
	c.bnez x9, 1f
	c.slli x8, 16
1:	c.srli x8, 16
	x.poke 6, x8
	c.j ret_rv32

load_form_address:
	c.mv x31, ra

	# x8 -> 32-bit address, possibly unaligned
	c.mv x8, x4
	c.srai x8, 20
	c.add x8, x18

	# x8 -> 32-bit address; x9 -> address LSBs
	c.mv x9, x8
	c.andi x9, 3
	c.andi x8, -4

	c.jr x31

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
	x.thunk 3

beq:
	c.mv x8, x18
	c.mv x9, x20
	c.sub x8, x9
	c.bnez x8, 1f /* branch not taken */

	c.jal resolve_imm1
	c.add x8, x3
	x.thunk 8

1:	c.j ret_rv32

bne:
	c.mv x8, x18
	c.mv x9, x20
	c.sub x8, x9
	c.beqz x8, 1f /* branch not taken */

	c.jal resolve_imm1
	c.add x8, x3
	x.thunk 8

1:	c.j ret_rv32

blt:
	c.mv x8, x18
	c.mv x9, x20
	c.jal slt_func
	c.beqz x8, 1f /* branch not taken */

	c.jal resolve_imm1
	c.add x8, x3
	x.thunk 8

1:	c.j ret_rv32

slt:
	c.mv x8, x18
	c.mv x9, x20
	c.jal slt_func
	x.poke 6, x8
	c.j ret_rv32

addi:	c.mv x8, x4
	c.srai x8, 20
	c.add x8, x18
	x.poke 6, x8
	c.j ret_rv32

andi:	c.mv x8, x4
	c.srai x8, 20
	c.mv x9, x18
	c.and x8, x9
	x.poke 6, x8
	c.j ret_rv32

ori:	c.mv x8, x4
	c.srai x8, 20
	c.mv x9, x18
	c.or x8, x9
	x.poke 6, x8
	c.j ret_rv32

xori:	c.mv x8, x4
	c.srai x8, 20
	c.mv x9, x18
	c.xor x8, x9
	x.poke 6, x8
	c.j ret_rv32

slti:
	c.mv x8, x4
	c.srai x8, 20
	c.mv x9, x8

	c.mv x8, x18
	c.jal slt_func
	x.poke 6, x8
	c.j ret_rv32

sltiu:
	c.mv x8, x4
	c.srai x8, 20
	c.mv x9, x8

	c.mv x8, x18
	c.jal sltu_func
	x.poke 6, x8
	c.j ret_rv32

bge:
	c.mv x8, x18
	c.mv x9, x20
	c.sub x8, x9

	c.li x9, 1
	c.slli x9, 31
	c.and x8, x9
	c.bnez x8, 1f /* branch not taken */

	c.jal resolve_imm1
	c.add x8, x3
	x.thunk 8

1:	c.j ret_rv32

bltu:
	c.mv x8, x18
	c.mv x9, x20

	c.jal sltu_func
	c.beqz x8, 1f

	/* take the branch */
	c.jal resolve_imm1
	c.add x8, x3
	x.thunk 8

1:	c.j ret_rv32

slt_func: /* clobbers x10, x11 */
	c.mv x31, ra

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

	c.jr x31

sltu_func: /* clobbers x10, x11 */
	c.mv x31, ra

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

	c.jr x31

bgeu:
	c.mv x8, x18
	c.mv x9, x20

	c.jal sltu_func
	c.bnez x8, 1f

	/* take the branch */
	c.jal resolve_imm1
	c.add x8, x3
	x.thunk 8

1:	c.j ret_rv32

add_sub:
	c.mv x8, x18
	c.mv x9, x20

	/* disambiguate add/sub */
	c.mv x10, x4
	c.li x11, 1
	c.slli x11, 30 /* 0x40000000 */
	c.and x10, x11
	c.beqz x10, 1f
	c.li x10, -1
	c.xor x9, x10
	c.addi x9, 1

1:	c.add x8, x9
	x.poke 6, x8
	c.j ret_rv32

.macro shifter rv32_inst rvc_inst
\rv32_inst:
	c.mv x8, x18

	/* One shamt bit at a time - 5 bits provides 32 bits' shift */
	c.mv x9, x20
	c.andi x9, 16
	c.beqz x9, 1f
	\rvc_inst x8, 16

1:	c.mv x9, x20
	c.andi x9, 8
	c.beqz x9, 2f
	\rvc_inst x8, 8

2:	c.mv x9, x20
	c.andi x9, 4
	c.beqz x9, 3f
	\rvc_inst x8, 4

3:	c.mv x9, x20
	c.andi x9, 2
	c.beqz x9, 4f
	\rvc_inst x8, 2

4:	c.mv x9, x20
	c.andi x9, 1
	c.beqz x9, 5f
	\rvc_inst x8, 1

5:	x.poke 6, x8
	c.j ret_rv32
.endm

	shifter sll c.slli
	shifter srl c.srli
	shifter sra c.srai

srl_sra:
	/* disambiguate srl/sra */
	c.mv x10, x4
	c.srli x10, 30
	c.beqz x10, srl
	c.j sra

slli:
	c.mv x20, x19
	c.j sll

srli_srai:
	c.mv x20, x19
	c.j srl_sra

xor:
	c.mv x8, x18
	c.mv x9, x20
	c.xor x8, x9
	x.poke 6, x8
	c.j ret_rv32

or:
	c.mv x8, x18
	c.mv x9, x20
	c.or x8, x9
	x.poke 6, x8
	c.j ret_rv32

and:
	c.mv x8, x18
	c.mv x9, x20
	c.and x8, x9
	x.poke 6, x8
	c.j ret_rv32

jalr:
	/* Save pc+4 to rd */
	c.mv x9, x3
	c.addi x9, 4
	x.poke 6, x9

	/* Resolve immediate and add to rd */
	c.mv x8, x4
	c.srai x8, 20
	c.add x8, x18
	c.andi x8, -2 /* zero LSB */

	/* Thunk there */
	x.thunk 8

jal:
	/* sign extend into imm[20] */
	c.mv x8, x4
	c.srai x8, 31
	c.srli x8, 20
	c.and x8, x9
	c.srai x8, 11
	c.mv x9, x8

	/* imm[19:12] */
	c.mv x8, x4
	c.slli x8, 12
	c.srli x8, 24
	c.slli x8, 12
	c.or x9, x8

	/* imm[11] */
	c.mv x8, x19
	c.andi x8, 1
	c.slli x8, 11
	c.or x9, x8

	/* imm[10:1] */
	c.mv x8, x4
	c.slli x8, 1
	c.srli x8, 21
	c.andi x8, -2
	c.or x8, x9

	/* Write return address into rd */
	c.mv x9, x3
	c.addi x9, 4
	x.poke 6, x9

	/* Form pc-relative offset and thunk there */
	c.add x8, x3
	x.thunk 8

resolve_imm1:
	c.mv x31, ra

	/* Signed immediate per BEQ and friends into x8; x9, x10, x31 destroyed */
	c.mv x8, x4
	c.srai x8, 31 /* sign extend MSB into imm[12] */
	c.slli x8, 12
	c.mv x9, x8

	/* pick imm[11] */
	c.mv x8, x6
	c.andi x8, 1
	c.slli x8, 11
	c.or x9, x8

	/* pick imm[10:5] */
	c.mv x8, x4
	c.slli x8, 1 /* knock off MSB */
	c.srli x8, 26 /* knock off LSBs */
	c.slli x8, 5 /* move into place */
	c.or x9, x8

	/* pick imm[4:1] */
	c.mv x8, x6
	c.andi x8, 0x1e /* mask LSB */
	c.or x8, x9

	c.jr x31

	/* avoids fetch-ahead metavalues in simulation - not really needed */
	c.nop
	c.nop
	c.nop
