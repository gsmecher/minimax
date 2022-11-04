#!/bin/bash

TC=${1:-*}

# Produce a crude histogram of clocks between trap/thunk instruction pairs.
# This is not representative, since it reflects the instruction weights of
# traps in the test suite, which is definitely not normal code.
awk '
	# Record stats for total CPI
	/SUCCESS/ { inside_test = 0 }
	/0004\s+0002\s+0000/ { inside_test = 1 }
	!/MCODE|BUBBLE/ { if(inside_test) { retired++ }}
	{ if(inside_test) ticks++ }

	# Record stats for emulated instructions
	/TRAP/ { mcode_start=FNR }
	/THUNK/ {
		a[FNR-mcode_start]++
		traps++
		mcode_ticks += FNR-mcode_start

		if(a[FNR-mcode_start] > longest)
			longest = a[FNR-mcode_start];
	}
	END {
		# Report emulation stats
		for(i in a) {
			printf("%.3i: %.3i\t", i, a[i]);
			for(j=0; j<50*a[i]/longest; j++) printf("*");
			printf("\n");
		}
		if(traps)
			printf("Emulation average: %f (%i ticks, %i traps)\n", mcode_ticks/traps, mcode_ticks, traps);
		printf("Overall CPI: %f (%i ticks, %i instructions)\n", ticks/retired, ticks, retired);
	}
' $TC/log
