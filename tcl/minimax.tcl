#!/usr/bin/env -S vivado -mode batch -source

create_project minimax minimax -part "xcku060-ffva1517-1-c"

# Set project properties
set_property -dict [ list				\
	default_lib xil_defaultlib			\
	ip_cache_permissions disable			\
	mem.enable_memory_map_generation 1		\
	part "xcku060-ffva1517-1-c"			\
	sim.ip.auto_export_scripts 1			\
	simulator_language Mixed			\
	target_language VHDL				\
	xpm_libraries "XPM_CDC XPM_MEMORY"		\
] -objects [current_project]

# Sources
add_files -fileset sources_1 [list			\
	[file normalize "../rtl/minimax.vhd"]		\
	[file normalize "../rtl/minimax_tb.vhd"]	\
]

set_property file_type "VHDL 2008" -objects [get_files -of_objects [get_filesets sources_1]]
start_gui
