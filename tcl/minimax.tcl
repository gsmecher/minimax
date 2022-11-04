#!/usr/bin/env -S vivado -mode batch -source

create_project minimax minimax -part "xcku035-fbva676-2-e"

# Set project properties
set_property -dict [ list				\
	default_lib xil_defaultlib			\
	ip_cache_permissions disable			\
	mem.enable_memory_map_generation 1		\
	part "xcku035-fbva676-2-e"			\
	sim.ip.auto_export_scripts 1			\
	simulator_language Mixed			\
	target_language VHDL				\
	xpm_libraries "XPM_CDC XPM_MEMORY"		\
] -objects [current_project]

# Sources
add_files -fileset sources_1 [list			\
	[file normalize "../rtl/minimax.vhd"]		\
	[file normalize "../rtl/blinker.vhd"]	\
	[file normalize "../rtl/minimax_tb.vhd"]	\
]
set_property file_type "VHDL 2008" -objects [get_files -of_objects [get_filesets sources_1]]

# Blinker assembly
add_files ../asm/blink.mem

set_property top blinker [current_fileset]

# Constraints - these inject a nominal clock only, and leave I/O placements up
# to the placer. As a result, Fmax estimates are pessimistic because routing
# delays are dominated by routes to I/Os.
add_files -fileset constrs_1 minimax.xdc

# Ensure we're aggressively optimizing
set_property STEPS.SYNTH_DESIGN.ARGS.DIRECTIVE AreaOptimized_high [get_runs synth_1]

start_gui
