--
-- Minimax: microcoded RISC-V
--
-- (c) 2022 Three-Speed Logic, Inc., all rights reserved.
--
-- This testbench contains:
--
-- * A minimax core,
-- * A 4kB dual-port RAM connected to both instruction and data buses, and
-- * Enough "peripheral" to halt the simulation on completion.
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.env.all;
use std.textio.all;

entity minimax_tb is
	generic (
		ROM_FILENAME : string := "minimax.rom";
		MAXTICKS : integer := -1;
		TRACE : boolean := TRUE);
end minimax_tb;

architecture behav of minimax_tb is
	signal clk : std_logic := '0';
	signal reset : std_logic := '0';

	signal inst_addr : std_logic_vector(12 downto 0);
	signal inst_lat, inst_reg : std_logic_vector(15 downto 0) := (others => '0');
	signal inst_regce : std_logic;

	signal addr, rdata, wdata : std_logic_vector(31 downto 0) := (others => '0');
	signal wmask : std_logic_vector(3 downto 0) := x"0";
	signal rreq : std_logic := '0';

	signal ticks : integer := 0;

	type rom_type is array(0 to 2047) of std_logic_vector(31 downto 0);

	impure function rom_init return rom_type is
		file f : text open read_mode is ROM_FILENAME;
		variable l : line;
		variable rom : rom_type;
		variable good : boolean := true;
		variable i : integer := 0;
	begin
		while not endfile(f) loop
			readline(f, l);
			hread(l, rom(i), good);
			assert good severity failure;
			i := i + 1;
		end loop;

		assert i > 0
			report "Failed to read any ROM data!"
			severity failure;

		return rom;
	end function;

	signal rom_array : rom_type := (others => 32x"0");
begin
	clk <= not clk after 10 ns;

	rom_proc : process
		variable i32 : std_logic_vector(31 downto 0);
	begin
		-- Clunky, but - we want to initialize ROM during simulation, not during elaboration.
		-- Otherwise, we'd have to create a different executable for each test case.
		rom_array <= rom_init;

		while True loop
			wait until rising_edge(clk);

			rdata <= rom_array(to_integer(unsigned(addr(inst_addr'high downto 2))));
			i32 := rom_array(to_integer(unsigned(inst_addr(inst_addr'high downto 2))));
			if inst_addr(1) then
				inst_lat <= i32(31 downto 16);
			else
				inst_lat <= i32(15 downto 0);
			end if;

			if inst_regce then
				inst_reg <= inst_lat;
			end if;

			if wmask=x"f" then
				rom_array(to_integer(unsigned(addr(inst_addr'high downto 2)))) <= wdata;
			end if;
		end loop;
	end process;

	dut : entity work.minimax
	generic map (
		PC_BITS => inst_addr'length,
		UC_BASE => x"00000800",
		TRACE => TRACE)
	port map (
		clk => clk,
		reset => reset,
		inst_addr => inst_addr,
		inst => inst_reg,
		inst_regce => inst_regce,
		addr => addr,
		wdata => wdata,
		rdata => rdata,
		wmask => wmask,
		rreq => rreq);

	stim : process
	begin
		reset <= '1';
		wait for 100 ns;
		reset <= '0';
		wait;
	end process;

	-- Capture test exit conditions - timeout or quit
	tb_proc: process(clk)
		variable buf : line;
	begin
		if rising_edge(clk) then
			-- Track ticks counter and bail if we took too long
			ticks <= ticks + 1;
			if MAXTICKS/=-1 and ticks >= MAXTICKS then
				write(buf, "FAIL: Exceeded MAXTICKS of " & integer'image(MAXTICKS));
				writeline(output, buf);
				finish(-1);
			end if;

			-- Capture writes to address 0xfffffffc and use these as "quit" values
			if wmask=x"f" and addr=x"fffffffc" then
				if to_integer(signed(wdata))=0 then
					write(buf, string'("SUCCESS: returned 0."));
				else
					write(buf, "FAIL: returned " & integer'image(to_integer(signed(wdata))));
				end if;
				writeline(output, buf);
				finish(to_integer(signed(wdata)));
			end if;
		end if;
	end process;
end behav;
