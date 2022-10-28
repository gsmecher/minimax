#include "test.inc"

_start:
	# Generate register values of 16 two independent ways, and ensure they agree
	c.li x8, 16
	c.li x9, 1
.rept 4
	c.slli x9, 1
.endr
	c.sub x9, x8
	c.bnez x9, fail

	# Test directly implemented RV32I lui
	lui x8, 0xabcde
.rept 12
	c.srli x8, 1
.endr
	c.li x9, 0xe
	c.xor x8, x9

	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.li x9, 0xd
	c.xor x8, x9

	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.li x9, 0xc
	c.xor x8, x9

	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.li x9, 0xb
	c.xor x8, x9

	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.li x9, 0xa
	c.xor x8, x9

	c.bnez x8, fail

	# Test directly implemented RV32I addi
	li x8, 0xdef
	c.li x9, 0xf
	c.xor x8, x9

	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.li x9, 0xe
	c.xor x8, x9

	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.srli x8, 1
	c.li x9, 0xd
	c.xor x8, x9

	c.bnez x8, fail

	# Test negative extrema (hence, sign extension)
	c.li x8, -32
	c.addi x8, 31
	c.addi x8, 1
	c.bnez x8, fail

	# Test positive extrema
	c.li x8, 31
	c.addi x8, -31
	c.bnez x8, fail

	# Test LUI
	c.lui x8, 0xf /* produces 0xf000 */
.rept 12
	c.srli x8, 1
.endr
	c.addi x8, -15
	c.bnez x8, fail

	# Test ADDI4SPN
	c.li sp, 0
	c.addi4spn x8, sp, 4 # produces 0x4
	c.addi x8, -4
	c.bnez x8, fail

	# Test ADDI16SP
	c.li sp, 0
	c.addi16sp sp, -0x10 # produces 0x10
	c.addi4spn x8, sp, 16 # cheeky mv and add
	c.bnez x8, fail

	succeed

fail:	fail
