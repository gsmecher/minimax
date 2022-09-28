#include "test.inc"

_start:
	# Clear x8, jump to a function that sets is, and check
	c.li x8, 0		# 0000
	c.jal set		# 0002
	c.addi x8, -1		# 0004
	c.bnez x8, fail		# 0006

	# Set x8, jump to a function that clears it, and check
	c.li x8, 1		# 0008
	c.jal clear		# 000a
	c.bnez x8, fail		# 000c
	c.j succeed		# 000e

set:
	c.li x8, 1		# 0010
	c.jr x1			# 0012

clear:
	c.li x8, 0		# 0014
	c.jr x1			# 0016

succeed:succeed
fail:	fail
