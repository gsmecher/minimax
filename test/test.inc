.macro succeed
	c.li x8, -4
	c.li x9, 0
	c.sw x9, 0(x8)
.endm

.macro fail
	c.li x8, -4
	c.li x9, 1
	c.sw x9, 0(x8)
.endm
