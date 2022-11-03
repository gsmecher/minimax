#!/bin/bash

# Produce a crude histogram of clocks between trap/thunk instruction pairs.
# This is not representative, since it reflects the instruction weights of
# traps in the test suite, which is definitely not normal code.
awk '
	/TRAP/ {
		start=FNR
	}
	/THUNK/ {
		a[FNR-start]++
		traps++
		ticks += FNR-start

		if(a[FNR-start] > longest)
			longest = a[FNR-start];
	}
	END {
		for(i in a) {
			printf("%.3i: %.3i\t", i, a[i]);
			for(j=0; j<50*a[i]/longest; j++) printf("*");
			printf("\n");
		}
		printf("Average: %f (%i ticks, %i traps)\n", ticks/traps, ticks, traps);
	}
' */log
