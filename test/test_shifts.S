#include "test.inc"

_start:
	#
	# Direct RV32 shift
	#
#if 0	
	c.li x8, -1
	c.li x9, 31
	sra x10, x8, x9
	c.addi x10, 1
	c.bnez x10, fail
#endif
	c.li x8, 1
	c.li x9, 31
	sll x8, x8, x9
	c.srli x8, 31
	c.addi x8, -1
	c.bnez x8, fail

	#
	# Normal shifts with shamt=1
	#

	# c.srai
	c.li x8, -1	# x8 <= 0xffffffff
	c.srai x8, 1
	c.addi x8, 1
	c.bnez x8, fail

	# c.srli and c.slli
	c.li x8, -1
	c.srli x8, 1	# x8 <= 0x7fffffff
	c.slli x8, 1	# x8 <= 0xfffffffe
	c.addi x8, 2
	c.bnez x8, fail

	# now c.srai with a shift of 2
	c.li x8, 4
	c.srai x8, 2
	c.addi x8, -1
	c.bnez x8, fail

	# try a shift all the way to 0
	c.li x8, 1
	c.slli x8, 31
	c.beqz x8, fail
	c.slli x8, 1
	c.bnez x8, fail

	# degenerate shifts: what if shamt=0?
	# FIXME: these are reserved for HINTs and the assembler refuses to
	# produce them
#if 0
	c.li x8, 1
	c.slli x8, 0
	c.addi x8, -1
	c.bnez x8, fail

	c.li x8, 1
	c.srli x8, 0
	c.addi x8, -1
	c.bnez x8, fail

	c.li x8, 1
	c.srai x8, 0
	c.addi x8, -1
	c.bnez x8, fail
#endif

	succeed

fail:	fail
