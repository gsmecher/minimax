#include "test.inc"

_start:
	c.li x2, 1

	/*
	 * SRAI
	 */

	/* Ensure the 32-bit instruction does not cross words - split
	 * instructions follow a more complex code path and we test it later. */
	c.nop
	slli x1, x2, 2
	c.addi x1, -4
	c.mv x8, x1
	c.bnez x8, fail

	/* Repeat, ensuring misaligned instructions */
	c.nop
	c.li x2, 1
	slli x1, x2, 2
	c.addi x1, -4
	c.mv x8, x1
	c.bnez x8, fail

	succeed

fail:	fail
