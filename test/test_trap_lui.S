#include "test.inc"

_start:
	lui x8, 0xbeefc

	/* test digit by digit */
	c.srli x8, 12
	c.mv x9, x8
	c.andi x9, 0xf
	c.addi x9, -0xc
	c.bnez x9, fail

	c.srli x8, 4
	c.mv x9, x8
	c.andi x9, 0xf
	c.addi x9, -0xf
	c.bnez x9, fail

	c.srli x8, 4
	c.mv x9, x8
	c.andi x9, 0xf
	c.addi x9, -0xe
	c.bnez x9, fail

	c.srli x8, 4
	c.mv x9, x8
	c.andi x9, 0xf
	c.addi x9, -0xe
	c.bnez x9, fail

	c.srli x8, 4
	c.mv x9, x8
	c.andi x9, 0xf
	c.addi x9, -0xb
	c.bnez x9, fail

	succeed

fail:	fail
