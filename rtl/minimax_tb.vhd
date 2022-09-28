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

library unisim;
use unisim.vcomponents.all;

library std;
use std.env.all;
use std.textio.all;

entity minimax_tb is
	generic (
		ROM : string := "minimax.rom";
		MAXTICKS : integer := -1);
end minimax_tb;

architecture behav of minimax_tb is
	signal clk : std_logic := '0';
	signal reset : std_logic := '0';

	signal inst_addr : std_logic_vector(12 downto 0);
	signal inst, inst_reg : std_logic_vector(15 downto 0) := (others => '0');
	signal inst_regce : std_logic;

	signal addr, rdata, wdata : std_logic_vector(31 downto 0) := (others => '0');
	signal wmask : std_logic_vector(3 downto 0) := x"0";
	signal rreq : std_logic := '0';

	signal ticks : integer := 0;

	type rom_type is array(0 to 2047) of std_logic_vector(31 downto 0);

	impure function rom_init return rom_type is
		file f : text open read_mode is ROM;
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
				inst <= i32(31 downto 16);
			else
				inst <= i32(15 downto 0);
			end if;

			if inst_regce then
				inst_reg <= inst;
			end if;

			if wmask=x"f" then
				rom_array(to_integer(unsigned(addr(inst_addr'high downto 2)))) <= wdata;
			end if;
		end loop;
	end process;

	dut : entity work.minimax
	generic map (
		PC_BITS => inst_addr'length,
		UC_BASE => x"00000800")
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

	-- Emit header
	process
		variable buf : line;
	begin
		write(buf, "FETCH1" & HT
			& "FETCH2" & HT
			& "EXECUTE" & HT
			& "aguA" & HT
			& "aguB" & HT
			& "aguX" & HT
			& "INST" & HT
			& "OPCODE" & HT
			& "addrD" & HT
			& "addrS" & HT
			& "regD" & HT & HT
			& "regS" & HT & HT
			& "aluA" & HT & HT
			& "aluB" & HT & HT
			& "aluS" & HT & HT
			& "aluX" & HT & HT
			& "Dnext");
		writeline(output, buf);
		wait;
	end process;

	-- Emit trace
	trace: process(clk)
		alias pc_fetch is << signal minimax_tb.dut.pc_fetch : unsigned(inst_addr'high downto 1) >>;
		alias pc_fetch_dly is << signal minimax_tb.dut.pc_fetch_dly : unsigned(inst_addr'high downto 1) >>;
		alias pc_execute is << signal minimax_tb.dut.pc_execute : unsigned(inst_addr'high downto 1) >>;
		alias aguA is << signal minimax_tb.dut.aguA : unsigned(inst_addr'high downto 1) >>;
		alias aguB is << signal minimax_tb.dut.aguB : unsigned(inst_addr'high downto 1) >>;
		alias aguX is << signal minimax_tb.dut.aguX : unsigned(inst_addr'high downto 1) >>;

		alias inst is << signal minimax_tb.dut.inst : std_logic_vector(15 downto 0) >>;

		alias reset is << signal minimax_tb.dut.reset : std_logic >>;
		alias branch_taken is << signal minimax_tb.dut.branch_taken : std_logic >>;
		alias bubble is << signal minimax_tb.dut.bubble : std_logic >>;
		alias wb is << signal minimax_tb.dut.wb : std_logic >>;
		alias microcode is << signal minimax_tb.dut.microcode : std_logic >>;

		alias addrD is << signal minimax_tb.dut.addrD : std_logic_vector(5 downto 0) >>;
		alias addrS is << signal minimax_tb.dut.addrS : std_logic_vector(5 downto 0) >>;

		alias regD is << signal minimax_tb.dut.regD : std_logic_vector(31 downto 0) >>;
		alias regS is << signal minimax_tb.dut.regS : std_logic_vector(31 downto 0) >>;

		alias aluA is << signal minimax_tb.dut.aluA : std_logic_vector(31 downto 0) >>;
		alias aluB is << signal minimax_tb.dut.aluB : std_logic_vector(31 downto 0) >>;
		alias aluS is << signal minimax_tb.dut.aluS : std_logic_vector(32 downto 0) >>;
		alias aluX is << signal minimax_tb.dut.aluX : std_logic_vector(31 downto 0) >>;
		alias Dnext is << signal minimax_tb.dut.Dnext : std_logic_vector(31 downto 0) >>;

		alias rs_reg is << signal minimax_tb.dut.rs_reg : std_logic_vector(5 downto 0) >>;
		alias rd_reg is << signal minimax_tb.dut.rd_reg : std_logic_vector(5 downto 0) >>;
		alias shamt is << signal minimax_tb.dut.shamt : unsigned(4 downto 0) >>;

		alias op16_addi4spn is << signal minimax_tb.dut.op16_addi4spn : std_logic >>;
		alias op16_lw is << signal minimax_tb.dut.op16_lw : std_logic >>;
		alias op16_sw is << signal minimax_tb.dut.op16_sw : std_logic >>;

		alias op16_addi is << signal minimax_tb.dut.op16_addi : std_logic >>;
		alias op16_jal is << signal minimax_tb.dut.op16_jal : std_logic >>;
		alias op16_li is << signal minimax_tb.dut.op16_li : std_logic >>;
		alias op16_addi16sp is << signal minimax_tb.dut.op16_addi16sp : std_logic >>;
		alias op16_lui is << signal minimax_tb.dut.op16_lui : std_logic >>;

		alias op16_srli is << signal minimax_tb.dut.op16_srli : std_logic >>;
		alias op16_srai is << signal minimax_tb.dut.op16_srai : std_logic >>;
		alias op16_andi is << signal minimax_tb.dut.op16_andi : std_logic >>;
		alias op16_sub is << signal minimax_tb.dut.op16_sub : std_logic >>;
		alias op16_xor is << signal minimax_tb.dut.op16_xor : std_logic >>;
		alias op16_or is << signal minimax_tb.dut.op16_or : std_logic >>;
		alias op16_and is << signal minimax_tb.dut.op16_and : std_logic >>;
		alias op16_j is << signal minimax_tb.dut.op16_j : std_logic >>;
		alias op16_beqz is << signal minimax_tb.dut.op16_beqz : std_logic >>;
		alias op16_bnez is << signal minimax_tb.dut.op16_bnez : std_logic >>;

		alias op16_slli is << signal minimax_tb.dut.op16_slli : std_logic >>;
		alias op16_lwsp is << signal minimax_tb.dut.op16_lwsp : std_logic >>;
		alias op16_jr is << signal minimax_tb.dut.op16_jr : std_logic >>;
		alias op16_mv is << signal minimax_tb.dut.op16_mv : std_logic >>;
		alias op16_ebreak is << signal minimax_tb.dut.op16_ebreak : std_logic >>;
		alias op16_jalr is << signal minimax_tb.dut.op16_jalr : std_logic >>;
		alias op16_add is << signal minimax_tb.dut.op16_add : std_logic >>;
		alias op16_swsp is << signal minimax_tb.dut.op16_swsp : std_logic >>;

		alias op32_lui is << signal minimax_tb.dut.op32_lui : std_logic >>;
		alias op32_auipc is << signal minimax_tb.dut.op32_auipc : std_logic >>;
		alias op32_addi is << signal minimax_tb.dut.op32_addi : std_logic >>;
		alias op32_andi is << signal minimax_tb.dut.op32_andi : std_logic >>;
		alias op32_ori is << signal minimax_tb.dut.op32_ori : std_logic >>;
		alias op32_xori is << signal minimax_tb.dut.op32_xori : std_logic >>;
		alias op32_sll is << signal minimax_tb.dut.op32_sll : std_logic >>;
		alias op32_slli is << signal minimax_tb.dut.op32_slli : std_logic >>;
		alias op32_srl_sra is << signal minimax_tb.dut.op32_srl_sra : std_logic >>;
		alias op32_srli_srai is << signal minimax_tb.dut.op32_srli_srai : std_logic >>;

		alias op32 is << signal minimax_tb.dut.op32 : std_logic >>;
		alias op32_trap is << signal minimax_tb.dut.op32_trap : std_logic >>;

		alias op16_slli_setrd is << signal minimax_tb.dut.op16_slli_setrd : std_logic >>;
		alias op16_slli_setrs is << signal minimax_tb.dut.op16_slli_setrs : std_logic >>;
		alias op16_slli_thunk is << signal minimax_tb.dut.op16_slli_thunk : std_logic >>;

		variable buf : line;
	begin
		if rising_edge(clk) then
			write(buf, to_hstring(pc_fetch & "0"));
			write(buf, HT & to_hstring(pc_fetch_dly & "0"));
			write(buf, HT & to_hstring(pc_execute & "0"));
			write(buf, HT & to_hstring(aguA & "0"));
			write(buf, HT & to_hstring(aguB & "0"));
			write(buf, HT & to_hstring(aguX & "0"));
			write(buf, HT & to_hstring(inst));
			write(buf, HT);

			if op16_ADDI4SPN then
				write(buf, string'("ADI4SPN")); -- shortened to fit in a tab stop
			elsif op16_LW then
				write(buf, string'("LW"));
			elsif op16_SW then
				write(buf, string'("SW"));
			elsif op16_ADDI then
				write(buf, string'("ADDI"));
			elsif op16_JAL then
				write(buf, string'("JAL"));
			elsif op16_LI then
				write(buf, string'("LI"));
			elsif op16_ADDI16SP then
				write(buf, string'("ADI16SP")); -- shortened to fit in a tab stop
			elsif op16_LUI then
				write(buf, string'("LUI"));
			elsif op16_SRLI then
				write(buf, string'("SRLI"));
			elsif op16_SRAI then
				write(buf, string'("SRAI"));
			elsif op16_ANDI then
				write(buf, string'("ANDI"));
			elsif op16_SUB then
				write(buf, string'("SUB"));
			elsif op16_XOR then
				write(buf, string'("XOR"));
			elsif op16_OR then
				write(buf, string'("OR"));
			elsif op16_AND then
				write(buf, string'("AND"));
			elsif op16_J then
				write(buf, string'("J"));
			elsif op16_BEQZ then
				write(buf, string'("BEQZ"));
			elsif op16_BNEZ then
				write(buf, string'("BNEZ"));
			elsif op16_SLLI then
				write(buf, string'("SLLI"));
			elsif op16_LWSP then
				write(buf, string'("LWSP"));
			elsif op16_JR then
				write(buf, string'("JR"));
			elsif op16_MV then
				write(buf, string'("MV"));
			elsif op16_EBREAK then
				write(buf, string'("EBREAK"));
			elsif op16_JALR then
				write(buf, string'("JALR"));
			elsif op16_ADD then
				write(buf, string'("ADD"));
			elsif op16_SWSP then
				write(buf, string'("SWSP"));
			elsif op16_SLLI_THUNK then
				write(buf, string'("THUNK"));
			elsif op16_SLLI_SETRD then
				write(buf, string'("SETRD"));
			elsif op16_SLLI_SETRS then
				write(buf, string'("SETRS"));
			elsif op32_lui then
				write(buf, string'("32LUI"));
			elsif op32_auipc then
				write(buf, string'("32AUIPC"));
			elsif op32_sll then
				write(buf, string'("32SL"));
			elsif op32_slli then
				write(buf, string'("32SLI"));
			elsif op32_srl_sra then
				write(buf, string'("32SR"));
			elsif op32_srli_srai then
				write(buf, string'("32SRI"));
			elsif op32_addi then
				write(buf, string'("32ADDI"));
			elsif op32_andi then
				write(buf, string'("32ANDI"));
			elsif op32_ori then
				write(buf, string'("32ORI"));
			elsif op32_xori then
				write(buf, string'("32XORI"));
			elsif op32 then
				write(buf, string'("RV32I"));
			else
				write(buf, string'("NOP?"));
			end if;

			write(buf, HT & to_hstring(addrD));
			write(buf, HT & to_hstring(addrS));

			write(buf, HT & to_hstring(regD));
			write(buf, HT & to_hstring(regS));

			write(buf, HT & to_hstring(aluA));
			write(buf, HT & to_hstring(aluB));
			write(buf, HT & to_hstring(aluS));
			write(buf, HT & to_hstring(aluX));

			write(buf, HT & to_hstring(Dnext));

			if op32_trap then
				write(buf, HT & "TRAP");
			end if;
			if branch_taken then
				write(buf, HT & "TAKEN");
			end if;
			if bubble then
				write(buf, HT & "BUBBLE");
			end if;
			if wb then
				write(buf, HT & "WB");
			end if;
			if reset then
				write(buf, HT & "RESET");
			end if;
			if microcode then
				write(buf, HT & "MCODE");
			end if;
			if(or wmask) then
				write(buf, HT & "WMASK=" & to_hstring(wmask));
				write(buf, HT & "ADDR=" & to_hstring(addr));
				write(buf, HT & "WDATA=" & to_hstring(wdata));
			end if;
			if(rreq) then
				write(buf, HT & "RREQ");
				write(buf, HT & "ADDR=" & to_hstring(addr));
			end if;
			if(or shamt) then
				write(buf, HT & "SHAMT=" & to_hstring(shamt));
			end if;
			if(or rd_reg) then
				write(buf, HT & "@RD=" & to_hstring(rd_reg));
			end if;
			if(or rs_reg) then
				write(buf, HT & "@RS=" & to_hstring(rs_reg));
			end if;
			writeline(output, buf);

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
