#include "test.inc"

_start:
	# Loop 10 times using bnez
	c.li x9, 0
loop:	c.addi x9, 1
	c.li x8, 10
	c.sub x8, x9
	c.bnez x8, loop

	# Jump over failure using beqz
	c.bnez x8, fail
	succeed

fail:	fail
