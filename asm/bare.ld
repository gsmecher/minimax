MEMORY {
	BRAM (RWX) : ORIGIN = 0x0, LENGTH = 0x1000
}

SECTIONS {
	. = 0x0;

	.text : {
		_start = .;
		*(.text)

		. = 0x800;
		*(.mctext*)
	} > BRAM

}
