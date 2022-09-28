void _start(void) {
	asm(
		"c.li x8, -4;"
		"c.li x9, 0;"
		"c.sw x9, 0(x8)");
}
