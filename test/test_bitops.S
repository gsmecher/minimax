#include "test.inc"

_start:
	# ANDI
	c.li x8, -1	# x8 <= 0xffffffff
	c.andi x8, 0xe	# xi <= 0xe
	c.addi x8, -14	# 0
	c.bnez x8, fail

	# AND
	c.li x8, 0xa
	c.li x9, 0x5
	c.and x8, x9
	c.bnez x8, fail

	# OR
	c.li x8, 0xa
	c.li x9, 0x5
	c.or x8, x9 # 0xf
	c.addi x8, -15
	c.bnez x8, fail

	# XOR
	c.li x8, 0xa
	c.li x9, 0x5
	c.xor x8, x9
	c.addi x8, -15
	c.bnez x8, fail

	succeed

fail:	fail
