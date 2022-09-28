#include "test.inc"

_start:
	c.li x2, 8

	/*
	 * SRAI
	 */

	/* Ensure the 32-bit instruction does not cross words - split
	 * instructions follow a more complex code path and we test it later. */
	c.nop
	srai x2, x2, 2
	c.addi x2, -2
	c.mv x8, x2
	c.bnez x8, fail

	/* Repeat, ensuring misaligned instructions */
	c.nop
	c.li x2, 8
	srai x2, x2, 2
	c.addi x2, -2
	c.mv x8, x2
	c.bnez x8, fail

	succeed

fail:	fail
