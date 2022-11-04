set_property -dict {					\
	CFGBVS GND					\
	CONFIG_VOLTAGE 1.8				\
	BITSTREAM.GENERAL.COMPRESS true			\
	BITSTREAM.CONFIG.UNUSEDPIN Pullup		\
	BITSTREAM.CONFIG.BPI_SYNC_MODE Type2		\
	CONFIG_MODE BPI16				\
} [current_design]

# 100 MHz system clock
create_clock -period 10.000 -name clk -waveform {0.000 5.000} [get_ports clk_p]
set_property -dict {LOC D18 IOSTANDARD LVDS} [get_ports clk_p]
set_property -dict {LOC C18 IOSTANDARD LVDS} [get_ports clk_n]

# LEDs
set_property -dict {LOC D26 IOSTANDARD LVCMOS18 SLEW SLOW DRIVE 12} [get_ports led]
