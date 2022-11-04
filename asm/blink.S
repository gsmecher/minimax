_start:
	li x9, 12499999	/* 1 Hz blinker @ 100 MHz clock */
	c.li x10, -4	/* 0xfffffffc -> port address */
	c.li x11, 0	/* current LED state */
1:	c.mv x8, x9	/* Initialize counter */

	/* Tight loop - 4 clocks per iteration
	 * (2 instructions + 2 branch-taken penalty cycles) */
2:	c.addi x8, -1
	c.bnez x8, 2b

	/* Toggle LED */
	c.addi x11, 1
	c.sw x11, 0(x10)
	c.j 1b
