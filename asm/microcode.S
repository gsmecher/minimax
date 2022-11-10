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
.macro x.swap rd
	.half 0x1022 + (\rd << 7)
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

	/* Hot path - we want to detect and emulate 16-bit SRLI/SRAI/SLLI
	 * opcodes as quickly as possible, since they are the only RVC
	 * instructions that aren't directly implemented. */

	/* Fetch instruction, which may be half-word aligned. */
	c.mv x15, x3
	c.andi x15, 3
	c.mv x8, x3
	c.andi x8, -4	/* strip LSBs and fetch */
	c.lw x9, 0(x8)
	c.beqz x15, 1f

	/* Half-aligned - if this is 32 bits, remember to grab the other 16 bits later */
	x.swap 9

	/* code paths re-join, with lower 16 bits of instruction in x9 */
1:	c.mv x10, x9
	c.andi x10, 3 /* select quadrant */
	c.slli x10, 1 /* 1 RVC opcode per table entry */

	/* Table jump */
	c.jal .+2
	c.addi ra, 6 /* offset to table base */
	c.add ra, x10
	c.jr ra

	/* Quadrant jump table */
	c.j . /* error - quadrant 0 is directly implemented in RTL */
	c.j op16_srai_srli
	c.j op16_slli
	c.j op32

op32:	/* Complete assembling the 32-bit opcode and unroll the 16-bit fastpath */
	c.mv x4, x9
	c.beqz x15, 1f

	/* Split instruction - fetch the other half and assemble */
	c.lui x11, 0x10
	c.addi x11, -1 /* 0xffff */
	c.and x9, x11

	c.lw x10, 4(x8)
	c.and x10, x11
	x.swap 10

	c.or x9, x10
	c.mv x4, x9
1:	c.j op32_entry

op16_shamt_x11:
	/* We don't need the opcode any more */
	c.srli x9, 1
	c.srli x9, 1

	/* Isolate shamt into x11 */
	c.mv x11, x9
	c.andi x11, 31

.rept 5
	c.srli x9, 1
.endr
	c.jr ra

op16_srai_srli:
	c.jal op16_shamt_x11

	/* isolate rd' into rd, stored in x12 */
	c.mv x12, x9
	c.andi x12, 0x7
	c.addi x12, 8

	/* fetch register value into x8 */
	x.peek x8, 12

	/* Disambiguate SRLI from SRAI */
	c.andi x9, 8
	c.beqz x9, 1f

	c.mv x9, x11
	c.jal srai_dyn
	c.j 2f

1:	c.mv x9, x11
	c.jal srli_dyn

	/* code paths rejoin */
2:
	/* Write back and thunk */
	x.poke 12, x8
	c.addi x3, 2
	x.thunk 3

op16_slli:
	c.jal op16_shamt_x11

	/* isolate rd, stored in x12 */
	c.mv x12, x9
	c.andi x12, 31

	/* fetch register value into x8 */
	x.peek x8, 12
	c.mv x9, x11
	c.jal slli_dyn

	/* Write back and thunk */
	x.poke 12, x8
	c.addi x3, 2
	x.thunk 3

srai_dyn: /* jump table POE. Trashes x15 */
	c.li x15, 16
	c.slli x15, 1 /* Form 2*(32-x9) for the jump table */
	c.sub x15, x9
	c.slli x15, 1
	c.mv x9, x15

	/* Table jump */
	c.mv x15, ra
	c.jal .+2
	c.addi ra, 8
	c.add ra, x9
	c.jalr ra
	c.jr x15

	/* JUMP TABLE - FORBIDDEN ZONE */

srai32:	c.srai x8, 1
srai31:	c.srai x8, 1
srai30:	c.srai x8, 1
srai29:	c.srai x8, 1
srai28:	c.srai x8, 1
srai27:	c.srai x8, 1
srai26:	c.srai x8, 1
srai25:	c.srai x8, 1
srai24:	c.srai x8, 1
srai23:	c.srai x8, 1
srai22:	c.srai x8, 1
srai21:	c.srai x8, 1
srai20:	c.srai x8, 1
srai19:	c.srai x8, 1
srai18:	c.srai x8, 1
srai17:	c.srai x8, 1
srai16:	c.srai x8, 1
srai15:	c.srai x8, 1
srai14:	c.srai x8, 1
srai13:	c.srai x8, 1
srai12:	c.srai x8, 1
srai11:	c.srai x8, 1
srai10:	c.srai x8, 1
srai9:	c.srai x8, 1
srai8:	c.srai x8, 1
srai7:	c.srai x8, 1
srai6:	c.srai x8, 1
srai5:	c.srai x8, 1
srai4:	c.srai x8, 1
srai3:	c.srai x8, 1
srai2:	c.srai x8, 1
srai1:	c.srai x8, 1
	c.jr ra

srli_dyn: /* jump table POE. Trashes x15 */
	c.li x15, 16
	c.slli x15, 1 /* Form 2*(32-x9) for the jump table */
	c.sub x15, x9
	c.slli x15, 1
	c.mv x9, x15

	/* Table jump */
	c.mv x15, ra
	c.jal .+2
	c.addi ra, 8
	c.add ra, x9
	c.jalr ra
	c.jr x15

	/* JUMP TABLE - FORBIDDEN ZONE */

srli32:	c.srli x8, 1
srli31:	c.srli x8, 1
srli30:	c.srli x8, 1
srli29:	c.srli x8, 1
srli28:	c.srli x8, 1
srli27:	c.srli x8, 1
srli26:	c.srli x8, 1
srli25:	c.srli x8, 1
srli24:	c.srli x8, 1
srli23:	c.srli x8, 1
srli22:	c.srli x8, 1
srli21:	c.srli x8, 1
srli20:	c.srli x8, 1
srli19:	c.srli x8, 1
srli18:	c.srli x8, 1
srli17:	c.srli x8, 1
srli16:	c.srli x8, 1
srli15:	c.srli x8, 1
srli14:	c.srli x8, 1
srli13:	c.srli x8, 1
srli12:	c.srli x8, 1
srli11:	c.srli x8, 1
srli10:	c.srli x8, 1
srli9:	c.srli x8, 1
srli8:	c.srli x8, 1
srli7:	c.srli x8, 1
srli6:	c.srli x8, 1
srli5:	c.srli x8, 1
srli4:	c.srli x8, 1
srli3:	c.srli x8, 1
srli2:	c.srli x8, 1
srli1:	c.srli x8, 1
	c.jr ra

slli_dyn: /* jump table POE. Trashes x15 */
	c.li x15, 16
	c.slli x15, 1 /* Form 2*(32-x9) for the jump table */
	c.sub x15, x9
	c.slli x15, 1
	c.mv x9, x15

	/* Table jump */
	c.mv x15, ra
	c.jal .+2
	c.addi ra, 8
	c.add ra, x9
	c.jalr ra
	c.jr x15

	/* JUMP TABLE - FORBIDDEN ZONE */

slli32:	c.slli x8, 1
slli31:	c.slli x8, 1
slli30:	c.slli x8, 1
slli29:	c.slli x8, 1
slli28:	c.slli x8, 1
slli27:	c.slli x8, 1
slli26:	c.slli x8, 1
slli25:	c.slli x8, 1
slli24:	c.slli x8, 1
slli23:	c.slli x8, 1
slli22:	c.slli x8, 1
slli21:	c.slli x8, 1
slli20:	c.slli x8, 1
slli19:	c.slli x8, 1
slli18:	c.slli x8, 1
slli17:	c.slli x8, 1
slli16:	c.slli x8, 1
slli15:	c.slli x8, 1
slli14:	c.slli x8, 1
slli13:	c.slli x8, 1
slli12:	c.slli x8, 1
slli11:	c.slli x8, 1
slli10:	c.slli x8, 1
slli9:	c.slli x8, 1
slli8:	c.slli x8, 1
slli7:	c.slli x8, 1
slli6:	c.slli x8, 1
slli5:	c.slli x8, 1
slli4:	c.slli x8, 1
slli3:	c.slli x8, 1
slli2:	c.slli x8, 1
slli1:	c.slli x8, 1
	c.jr ra

op32_entry:
	c.mv x8, x4

1:	/* Isolate opcode into x5 - note we strip the lower bits, which are always 11 */
	c.srli x8, 1
	c.srli x8, 1
	c.mv x9, x8
	c.andi x9, 0x1f
	c.mv x5, x9

	/* Isolate rd */
	c.jal srli5
	c.mv x9, x8
	c.andi x9, 0x1f
	c.mv x6, x9

	/* isolate funct3, left shifted by 1 for jump tables */
	c.jal srli5
	c.mv x9, x8
	c.andi x9, 0x7
	c.slli x9, 1
	c.mv x16, x9

	/* isolate rs1 */
	c.jal srli3
	c.mv x9, x8
	c.andi x9, 0x1f
	c.mv x17, x9

	/* look up rs1 value from register file (we mostly need it) */
	x.peek x18, 17

	/* isolate rs2/shamt */
	c.jal srli5
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
	c.j sltu	/* c.3: SLTU */
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
	c.jal srli12
	c.jal slli12
	x.poke 6, x8
	c.j ret_rv32

auipc:	c.mv x8, x4
	c.jal srli12
	c.jal slli12
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
2:	c.jal slli8
	c.addi x9, 1
	c.bnez x9, 2b

3:	c.jal srai24
	x.poke 6, x8
	c.j ret_rv32

lh:	c.jal load_form_address
	c.lw x8, 0(x8)
	c.bnez x9, 1f
	c.jal slli16
1:	c.jal srai16
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
2:	c.jal slli8
	c.addi x9, 1
	c.bnez x9, 2b

3:	c.jal srli24
	x.poke 6, x8
	c.j ret_rv32


lhu:	c.jal load_form_address
	c.lw x8, 0(x8)
	c.bnez x9, 1f
	c.jal slli16
1:	c.jal srli16
	x.poke 6, x8
	c.j ret_rv32

load_form_address:
	c.mv x31, ra

	# x8 -> 32-bit address, possibly unaligned
	c.mv x8, x4
	c.jal srai20
	c.add x8, x18

	# x8 -> 32-bit address; x9 -> address LSBs
	c.mv x9, x8
	c.andi x9, 3
	c.andi x8, -4

	c.jr x31

sw:	c.mv x8, x4
	c.jal srai20
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

sltu:
	c.mv x8, x18
	c.mv x9, x20
	c.jal sltu_func
	x.poke 6, x8
	c.j ret_rv32

addi:	c.mv x8, x4
	c.jal srai20
	c.add x8, x18
	x.poke 6, x8
	c.j ret_rv32

andi:	c.mv x8, x4
	c.jal srai20
	c.mv x9, x18
	c.and x8, x9
	x.poke 6, x8
	c.j ret_rv32

ori:	c.mv x8, x4
	c.jal srai20
	c.mv x9, x18
	c.or x8, x9
	x.poke 6, x8
	c.j ret_rv32

xori:	c.mv x8, x4
	c.jal srai20
	c.mv x9, x18
	c.xor x8, x9
	x.poke 6, x8
	c.j ret_rv32

slli:
	c.mv x8, x18
	c.mv x9, x19
	c.jal slli_dyn
	x.poke 6, x8
	c.j ret_rv32

srli_srai:
	/* Set up operands */
	c.mv x8, x18
	c.mv x9, x19

	/* disambiguate srl/sra */
	c.mv x10, x4
	c.mv x11, x4

	c.slli x11, 1
	c.slli x11, 1
	c.srli x11, 1
	c.srli x11, 1
	c.xor x11, x10
	c.beqz x11, 1f

	c.jal srai_dyn
	c.j 2f

1:	c.jal srli_dyn

2:	x.poke 6, x8
	c.j ret_rv32

slti:
	c.mv x8, x4
	c.jal srai20
	c.mv x9, x8

	c.mv x8, x18
	c.jal slt_func
	x.poke 6, x8
	c.j ret_rv32

sltiu:
	c.mv x8, x4
	c.jal srai20
	c.mv x9, x8

	c.mv x8, x18
	c.jal sltu_func
	x.poke 6, x8
	c.j ret_rv32

bge:
	c.mv x8, x18
	c.mv x9, x20
	c.sub x8, x9

	/* Form 0x80000 */
	c.lui x9, 0x10
	c.slli x9, 1
	c.slli x9, 1
	c.slli x9, 1

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
	c.xor x8, x9
	c.jal srli31
	c.beqz x8, 1f

	/* MSBs differed: right-shift to avoid overflow */
	c.srai x10, 1
	c.srai x9, 1

1:	/* MSBs were the same. Compare directly. */
	c.sub x10, x9
	c.mv x8, x10
	c.jal srli31

	c.jr x31

sltu_func: /* clobbers x10, x11 */
	c.mv x31, ra

	/* Compare MSBs */
	c.mv x10, x8
	c.xor x8, x9
	c.jal srli31
	c.beqz x8, 1f

	/* MSBs differed: right-shift to avoid overflow */
	c.srli x10, 1
	c.srli x9, 1

1:	/* MSBs were the same. Compare directly. */
	c.sub x10, x9
	c.mv x8, x10
	c.jal srli31

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
	c.mv x11, x4
	c.slli x11, 1
	c.slli x11, 1
	c.srli x11, 1
	c.srli x11, 1
	c.sub x10, x11

	c.beqz x10, 1f
	c.li x10, -1
	c.xor x9, x10
	c.addi x9, 1

1:	c.add x8, x9
	x.poke 6, x8
	c.j ret_rv32

sll:
	c.mv x8, x18
	c.mv x9, x20
	c.andi x9, 31
	c.jal slli_dyn
	x.poke 6, x8
	c.j ret_rv32

srl_sra:
	/* Cheat by leveraging srli_srai */
	c.mv x8, x20
	c.andi x8, 31
	c.mv x19, x8
	c.j srli_srai

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
	c.jal srai20
	c.add x8, x18
	c.andi x8, -2 /* zero LSB */

	/* Thunk there */
	x.thunk 8

jal:
	/* sign extend into imm[20] */
	c.mv x8, x4
	c.jal srai32
	c.jal srli20
	c.mv x9, x8

	/* imm[19:12] */
	c.lui x8, 0x1f /* 0x1f000 */
	c.jal slli3
	c.lui x10, 0x7
	c.or x10, x8 /* form 0xff000 */

	c.mv x8, x4
	c.and x8, x10
	c.or x9, x8

	/* imm[11] */
	c.mv x8, x19
	c.andi x8, 1
	c.jal slli11
	c.or x9, x8

	/* imm[10:1] */
	c.mv x8, x4
	c.slli x8, 1
	c.jal srli21
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
	c.jal srai31
	c.jal slli12 /* sign extend into imm[12] */
	c.mv x9, x8

	/* pick imm[11] */
	c.mv x8, x6
	c.andi x8, 1
	c.jal slli11
	c.or x9, x8

	/* pick imm[10:5] */
	c.mv x8, x4
	c.slli x8, 1
	c.jal srli26
	c.jal slli5
	c.or x9, x8

	/* pick imm[4:1] */
	c.mv x8, x6
	c.andi x8, 0x1e /* mask LSB */
	c.or x8, x9

	c.jr x31
