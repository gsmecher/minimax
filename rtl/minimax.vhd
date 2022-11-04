--
-- Minimax: microcoded RISC-V
--
-- (c) 2022 Three-Speed Logic, Inc., all rights reserved.
--
--
-- RISC-V's compressed instruction (RVC) extension is intended as an add-on to
-- the regular, 32-bit instruction set, not a replacement or competitor. Its
-- designers designed RVC instructions to be expanded into regular 32-bit RV32I
-- equivalents via a pre-decoder.
-- 
-- What happens if we *explicitly* architect a RISC-V CPU to execute RVC
-- instructions, and "mop up" any RV32I instructions that aren't convenient via
-- a microcode layer? What architectural optimizations are unlocked as a result?

-- "Minimax" is an experimental RISC-V implementation intended to establish if
-- an RVC-optimized CPU is, in practice, any simpler than an ordinary RV32I
-- core with pre-decoder. While it passes a modest test suite, you should not
-- use it without caution. (There are a large number of excellent, open source,
-- "little" RISC-V implementations you should probably use reach for first.)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity minimax is
generic (
	constant PC_BITS : natural := 12;
	constant UC_BASE : unsigned(31 downto 0) := x"00000000";
	constant TRACE : boolean := FALSE);
port (
	clk : in std_logic;
	reset : in std_logic;

	-- Program (including microcode) is accessed through here.
	-- Must be two clock cycles' latency.
	inst_addr : out std_logic_vector(PC_BITS-1 downto 0);
	inst : in std_logic_vector(15 downto 0);
	inst_regce : out std_logic := '0';

	addr : out std_logic_vector(31 downto 0) := (others => '0');
	wdata :  out std_logic_vector(31 downto 0) := (others => '0');
	wmask : out std_logic_vector(3 downto 0) := (others => '0');
	rdata :  in std_logic_vector(31 downto 0);
	rreq : out std_logic := '0');
end minimax;

architecture behav of minimax is
	function reversed (a: in std_logic_vector) return std_logic_vector is
		variable result: std_logic_vector(a'range);
		alias ar: std_logic_vector(a'reverse_range) is a;
	begin
		for i in ar'range loop
			result(i) := ar(i);
		end loop;
		return result;
	end;

	-- Register file
	type reg_array is array(integer range 0 to 63) of std_logic_vector(31 downto 0);
	signal register_file : reg_array := (others => (others => '0'));
	attribute RAM_STYLE : string;
	attribute RAM_STYLE of register_file : signal is "distributed";

	-- Register file address ports
	signal addrS, addrD : std_logic_vector(5 downto 0);
	signal regS, regD, aluA, aluB, aluS, aluX : std_logic_vector(31 downto 0); -- datapath

	-- Program counter
	signal pc_fetch, pc_fetch_dly, pc_execute : unsigned(PC_BITS-1 downto 1) := (others => '0');

	-- PC ALU output
	signal aguX, aguA, aguB : unsigned(PC_BITS-1 downto 1) := (others => '0');

	-- Track bubbles and execution inhibits through the pipeline.
	signal bubble, bubble1, bubble2 : std_logic := '1';
	signal branch_taken, microcode : std_logic := '0';
	signal trap: std_logic := '0';

	-- Writeback and deferred writeback strobes
	signal wb : std_logic := '0';
	signal dra : std_logic_vector(4 downto 0) := (others => '0'); -- deferred register address

	-- Strobes for 16-bit instructions
	signal op16 : std_logic := '0';
	signal op16_addi4spn : std_logic := '0';
	signal op16_lw, dly16_lw : std_logic := '0';
	signal op16_sw : std_logic := '0';

	signal op16_addi : std_logic := '0';
	signal op16_jal : std_logic := '0';
	signal op16_li : std_logic := '0';
	signal op16_addi16sp : std_logic := '0';
	signal op16_lui : std_logic := '0';

	signal op16_srli : std_logic := '0';
	signal op16_srai : std_logic := '0';
	signal op16_andi : std_logic := '0';
	signal op16_sub : std_logic := '0';
	signal op16_xor : std_logic := '0';
	signal op16_or : std_logic := '0';
	signal op16_and : std_logic := '0';
	signal op16_j : std_logic := '0';
	signal op16_beqz : std_logic := '0';
	signal op16_bnez : std_logic := '0';

	signal op16_slli : std_logic := '0';
	signal op16_lwsp, dly16_lwsp : std_logic := '0';
	signal op16_jr : std_logic := '0';
	signal op16_mv : std_logic := '0';

	signal op16_ebreak : std_logic := '0';
	signal op16_jalr : std_logic := '0';
	signal op16_add : std_logic := '0';
	signal op16_swsp : std_logic := '0';

	signal op16_slli_setrd, dly16_slli_setrd : std_logic := '0';
	signal op16_slli_setrs, dly16_slli_setrs : std_logic := '0';
	signal op16_slli_thunk : std_logic := '0';

	-- Strobes for 32-bit instructions
	signal op32 : std_logic := '0';

	signal shift_input, shifted : std_logic_vector(32 downto 0);
	signal shift_output : std_logic_vector(31 downto 0);
	signal shamt : integer range 0 to 31;
begin
	-- From 16.8 (RVC Instruction Set Listings)
	op16_addi4spn	<= inst ?= b"000_-_-----_-----_00" and not bubble;
	op16_lw		<= inst ?= b"010_-_-----_-----_00" and not bubble;
	op16_sw		<= inst ?= b"110_-_-----_-----_00" and not bubble;

	op16_addi	<= inst ?= b"000_-_-----_-----_01" and not bubble;
	op16_jal	<= inst ?= b"001_-_-----_-----_01" and not bubble;
	op16_li		<= inst ?= b"010_-_-----_-----_01" and not bubble;
	op16_addi16sp	<= inst ?= b"011_-_00010_-----_01" and not bubble;
	op16_lui	<= inst ?= b"011_-_-----_-----_01" and not bubble and not op16_addi16sp;

	op16_srli	<= inst ?= b"100_0_00---_-----_01" and not bubble;
	op16_srai	<= inst ?= b"100_0_01---_-----_01" and not bubble;
	op16_andi	<= inst ?= b"100_-_10---_-----_01" and not bubble;
	op16_sub	<= inst ?= b"100_-_11---_00---_01" and not bubble;
	op16_xor	<= inst ?= b"100_-_11---_01---_01" and not bubble;
	op16_or		<= inst ?= b"100_-_11---_10---_01" and not bubble;
	op16_and	<= inst ?= b"100_-_11---_11---_01" and not bubble;
	op16_j		<= inst ?= b"101_-_-----_-----_01" and not bubble;
	op16_beqz	<= inst ?= b"110_-_-----_-----_01" and not bubble;
	op16_bnez	<= inst ?= b"111_-_-----_-----_01" and not bubble;

	op16_slli	<= inst ?= b"000_0_-----_-----_10" and not bubble;
	op16_lwsp	<= inst ?= b"010_-_-----_-----_10" and not bubble;
	op16_jr		<= inst ?= b"100_0_-----_00000_10" and not bubble;
	op16_mv		<= inst ?= b"100_0_-----_-----_10" and not bubble and not op16_jr;
	op16_ebreak	<= inst ?= b"100_1_00000_00000_10" and not bubble;
	op16_jalr	<= inst ?= b"100_1_-----_00000_10" and not bubble and not op16_ebreak;
	op16_add	<= inst ?= b"100_1_-----_-----_10" and not bubble and not op16_jalr and not op16_ebreak;
	op16_swsp	<= inst ?= b"110_-_-----_-----_10" and not bubble;

	-- Non-standard extensions to support microcode are permitted in these opcode gaps
	op16_slli_setrd	<= inst ?= b"000_1_-----_00001_10" and not bubble;
	op16_slli_setrs	<= inst ?= b"000_1_-----_00010_10" and not bubble;
	op16_slli_thunk	<= inst ?= b"000_1_-----_00100_10" and not bubble;

	-- Blanket matches for RVC and RV32I instructions
	op32 <= and inst(1 downto 0) and not bubble;

	trap <= op32;

	assert UC_BASE(31 downto PC_BITS)=0
		report "Microcode at " & to_hstring(UC_BASE) & " cannot be reached with a " & integer'image(PC_BITS) & "-bit program counter!"
		severity failure;

	-- Data bus outputs tap directly off register/ALU path.
	wdata <= regD;
	addr <= std_logic_vector(aluS(addr'range));
	rreq <= op16_LWSP or op16_LW;
	wmask <= x"f" and (op16_SWSP or op16_SW);

	-- Instruction bus outputs do too
	inst_addr <= std_logic_vector(pc_fetch & "0");

	-- PC logic
	bubble <= bubble1 or bubble2;

	branch_taken <= (op16_BEQZ and (nor regS))
		or (op16_BNEZ and (or regS))
		or op16_J or op16_JAL or op16_JR or op16_JALR
		or op16_SLLI_THUNK;

	fetch_proc : process(clk)
	begin
		if rising_edge(clk) then
			-- Update fetch instruction unless we're hung up on a multi-cycle instruction word
			pc_fetch <= aguX and not reset;

			-- Fetch misses create a 2-cycle penalty
			bubble2 <= reset or branch_taken or trap;

			-- Multi-cycle instructions must correctly pause the fetch/execution pipeline
			bubble1 <= reset or bubble2 or rreq;

			if rreq then
				inst_regce <= '0';
			else
				pc_fetch_dly <= pc_fetch;
				pc_execute <= pc_fetch_dly;
				inst_regce <= '1';
			end if;

			microcode <= (microcode or trap) and not (reset or op16_SLLI_THUNK);

			assert not (microcode and trap)
				report "Double trap!"
				severity failure;
		end if;
	end process;

	datapath_proc : process(clk)
	begin
		if rising_edge(clk) then
			dly16_lw <= op16_lw;
			dly16_lwsp <= op16_lwsp;
			dly16_slli_setrs <= op16_slli_setrs;
			dly16_slli_setrd <= op16_slli_setrd;

			-- Load and setrs/setrd instructions complete a cycle after they are
			-- initiated, so we need to keep some state.
			dra <= (regD(4 downto 0) and (op16_SLLI_SETRD or op16_SLLI_SETRS))
				or (("01" & inst(4 downto 2)) and op16_LW)
				or ((inst(11 downto 7)) and op16_LWSP);
		end if;
	end process;

	-- READ/WRITE register file port
	addrD(4 downto 0) <= (dra and (dly16_SLLI_SETRD or dly16_LW or dly16_LWSP))
			or (5ux"01" and (op16_JAL or op16_JALR or trap)) -- write return address into ra
			or (("01" & inst(4 downto 2)) and (op16_ADDI4SPN or op16_SW)) -- data
			or (inst(6 downto 2) and op16_SWSP)
			or (inst(11 downto 7) and (op16_ADDI or op16_ADD
				or (op16_MV and not dly16_slli_setrd)
				or op16_ADDI16SP
				or op16_SLLI_SETRD or op16_SLLI_SETRS
				or op16_LI or op16_LUI
				or op16_SLLI))
			or (("01" & inst(9 downto 7)) and (op16_SUB
				or op16_XOR or op16_OR or op16_AND or op16_ANDI
				or op16_SRLI or op16_SRAI));

	-- READ-ONLY register file port
	addrS(4 downto 0) <= (dra and dly16_SLLI_SETRS)
			or (5ux"02" and (op16_ADDI4SPN or op16_LWSP or op16_SWSP))
			or (inst(11 downto 7) and (op16_JR or op16_JALR or op16_slli_thunk)) -- jump destination
			or (("01" & inst(9 downto 7)) and (op16_SW or op16_LW or op16_BEQZ or op16_BNEZ))
			or (("01" & inst(4 downto 2)) and (op16_AND or op16_OR or op16_XOR or op16_SUB))
			or (inst(6 downto 2) and ((op16_MV and not dly16_slli_setrs) or op16_ADD));

	-- Select between "normal" and "microcode" register banks.
	addrD(5) <= (microcode xor dly16_slli_setrd) or trap;
	addrS(5) <= microcode xor dly16_slli_setrs;

	-- Look up register file contents combinatorially
	regD <= register_file(to_integer(unsigned(addrD)));
	regS <= register_file(to_integer(unsigned(addrS)));

	-- Shifter coupled to regD. Vivado synthesizes something reasonable without intrusive help.
	shamt <= to_integer(unsigned(inst(6 downto 2)));
	shift_input(32) <= op16_srai and regD(31);
	shift_input(aluA'range) <= reversed(regD) when op16_slli else regD;
	shifted <= std_logic_vector(signed(shift_input) sra shamt);
	shift_output <= reversed(shifted(31 downto 0)) when op16_slli else shifted(31 downto 0);

	aluA <= (regD and (op16_ADD or op16_ADDI or op16_SUB
			or op16_AND or op16_ANDI
			or op16_OR or op16_XOR
			or op16_ADDI16SP))
		or ((22x"0" & inst(10 downto 7) & inst(12 downto 11) & inst(5) & inst(6) & "00") and op16_ADDI4SPN)
		or ((std_logic_vector'(31 downto 8 => '0') & inst(8 downto 7) & inst(12 downto 9) & "00") and op16_SWSP)
		or ((std_logic_vector'(31 downto 8 => '0') & inst(3 downto 2) & inst(12) & inst(6 downto 4) & "00") and op16_LWSP)
		or ((25x"0" & inst(5) & inst(12 downto 10) & inst(6) & "00") and (op16_LW or op16_SW));

	aluB <= regS
		or ((std_logic_vector'(31 downto 5 => inst(12)) & inst(6 downto 2)) and (op16_ADDI or op16_ANDI or op16_LI))
		or ((std_logic_vector'(31 downto 17 => inst(12)) & inst(6 downto 2) & 12x"0") and op16_LUI)
		or ((std_logic_vector'(31 downto 9 => inst(12)) & inst(4 downto 3) & inst(5) & inst(2) & inst(6) & x"0") and op16_ADDI16SP);

	-- This synthesizes into 4 CARRY8s - no need for manual xor/cin heroics
	aluS <= std_logic_vector(signed(aluA) - signed(aluB)) when op16_SUB
		else std_logic_vector(signed(aluA) + signed(aluB));

	-- ALU output multiplexer. The adder path (which is deep) must run
	-- parallel to the shifter path (which is also deep).
	aluX <= (aluS and (op16_ADD or op16_SUB or op16_ADDI or op16_ADDI4SPN or op16_ADDI16SP)) or
		(shift_output and (op16_slli or op16_srli or op16_srai)) or
		((aluA and aluB) and (op16_ANDI or op16_AND)) or
		((aluA xor aluB) and op16_XOR) or
		((aluA or aluB) and (op16_OR or op16_MV or op16_LI or op16_LUI)) or
		(rdata and (dly16_lw or dly16_lwsp)) or
		(std_logic_vector(resize(pc_fetch_dly & "0", 32) and (op16_JAL or op16_JALR or trap))); -- instruction following the jump (hence _dly)

	-- Address Generation Unit (AGU)

	aguA <= (pc_fetch and not (trap or branch_taken))
		or (pc_execute and branch_taken and not (op16_JR or op16_JALR or op16_slli_thunk));

	aguB <= (unsigned(regS(aguB'range)) and (op16_JR or op16_JALR or op16_slli_thunk))
		or unsigned((std_logic_vector'(aguB'high downto 11 => inst(12)) & inst(8) & inst(10 downto 9) & inst(6) & inst(7) & inst(2) & inst(11) & inst(5 downto 3))
			and (op16_J or op16_JAL))
		or unsigned((std_logic_vector'(aguB'high downto 8 => inst(12)) & inst(6 downto 5) & inst(2) & inst(11 downto 10) & inst(4 downto 3))
			and branch_taken and (op16_BNEZ or op16_BEQZ))
		or (unsigned(UC_BASE(pc_fetch'range)) and trap);

	aguX <= (aguA + aguB) + not (branch_taken or rreq or trap);

	wb <= trap or				-- writes microcode x1/ra
		dly16_lw or dly16_lwsp or	-- writes data
		op16_JAL or op16_JALR or	-- writes x1/ra
		op16_LI or op16_LUI or
		op16_ADDI or op16_ADDI4SPN or op16_ADDI16SP or
		op16_ANDI or op16_MV or op16_ADD or
		op16_AND or op16_OR or op16_XOR or op16_SUB or
		op16_SLLI or op16_SRLI or op16_SRAI;

	regs_proc : process(clk)
	begin
		if rising_edge(clk) then
			-- writeback
			if (or addrD(4 downto 0)) and wb then
				register_file(to_integer(unsigned(addrD))) <= aluX;
			end if;
		end if;
	end process;

	--
	-- The following trace code has no impact or cost in synthesis - it
	-- exists only to produce an execution trace for debugging.
	--
	-- pragma synthesis_off
	process
		variable buf : line;
	begin
		if TRACE then
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
				& "aluX");
			writeline(output, buf);
		end if;
		wait;
	end process;

	trace_proc : process(clk)
		variable buf : line;
	begin
		if TRACE and rising_edge(clk) then
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

			if trap then
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
			if(or dra) then
				write(buf, HT & "@DRA=" & to_hstring(dra));
			end if;
			writeline(output, buf);

		end if;
	end process;
	-- pragma synthesis_on
end behav;
