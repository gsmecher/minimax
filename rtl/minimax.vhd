--
-- Minimax: microcoded RISC-V
--
-- (c) 2022 Three-Speed Logic, Inc., all rights reserved.
--
-- In short:
--
-- * RV32C (compressed) instructions are first-class and execute at 1 clock per
--   instruction. (Exceptions: shifts and branches.)
--
-- * SOME RV32I instructions are directly implemented in RTL and execute in 2
--   clocks per instruction.
--
-- * Other RV32I instructions are emulated in microcode, using the instructions
--   above.
--
-- This is distinct from (all?) other RV32C-capable RISC-V cores, because it
-- really is architected for compressed first. This is not how the compressed
-- ISA was intended to be implemented.
--
-- A compressed-first RISC-V architecture unlocks the following:
--
-- * 1 clock per instruction (CPI) using a 2-port register file. RVC
--   instructions have only 1 rd and 1 rs field. A 2-port register file
--   maps cleanly into a single RAM64X1D per bit.
--
-- * A 16-bit instruction path. The processor is a modified
--   Harvard architecture, with a separate 16-bit instruction bus intended
--   to connect to a second port of the instruction memory. On Xilinx, the
--   asymmetric ports (16-bit instruction, 32-bit data) are reconciled
--   using an asymmetric block RAM primitive. As a result, we don't have to
--   worry about a 32-bit instruction split across two 32-bit words.
--
-- Native instructions are selected for a balance of:
--
-- * Small implementation cost. We use a 2-port register file, and 3-operand
--   instructions require more ports (or more cycles and logic to share ports)
--
-- * A reasonable performance baseline. Microcode traps require many clock
--   cycles, so an impoverished "direct" instruction set hurts us in two ways:
--
--   1. by requiring more traps to microcode emulation, and
--   2. by making microcode itself longer, due to an impoverished instruction
--      set.
--
-- * Sufficiency. Some RV32I instructions can't be emulated using just RV32C
--   and require RTL support. (For example: variable shifts, word/byte loads
--   and stores).
--
-- We end up with the following native instructions:
--
-- * C.xxx (all RV32C)
-- * LUI, AUIPC
-- * ADDI/NOP, ANDI, ORI, XORI
-- * SLLI, SRLI, SRAI
-- * SLL, SRL, SRA (these ones can't be implemented in RV32C alone)
--
-- Why is this desirable?
--
-- * Compilers (GCC, LLVM) are learning to prefer RVC instructions when
--   optimizing for size. This means compiled code (with appropriate
--   optimization settings) plays to Minimax's performance sweet-spot,
--   preferring direct instructions to microcoded instructions.
--   (see e.g. https://muxup.com/2022q3/whats-new-for-risc-v-in-llvm-15)
--
-- * RVC instructions nearly double code density, which pay for the cost of
--   microcode ROM when compared against a minimalist RV32I implementation.
--
-- * It's not quite the smallest RVC implementation (SERV is smaller), but
--   it is significantly faster with the appropriate compiler settings.
--
-- What's awkward?
--
-- * RVC decoding is definitely uglier than regular RV32I. I expect this
--   ugliness is better masked when RVC instructions are decoded to RV32I and
--   executed as "regular" 32-bit instructions.
--
-- What's the design like?
--
-- * Three-stage pipeline (fetch, fetch2, and everything-else). The fetch
--   pipeline is 2 cycles long to allow the use of embedded block RAM
--   registers, which frees up more clock slack for the execution stage.
--   There is a corresponding 2-cycle penalty on taken branches.
--
-- * Several "extension instructions" that use the non-standard extension space
--   reserved in C.SLLI. This space allows us to add "fused" instructions
--   accessible only in microcode, that perform the following:
--
--   - "Thunk" from microcode back to standard code,
--   - Move data from "user" registers into "microcode" registers and back again.
--
--   Because these extension instructions reach deeply into the implementation
--   details, they are ignored (converted to NOPs) outside emulation microcode.
--
-- Resource usage (excluding ROM and peripherals; KU060; 12-bit PC):
--
-- * Minimax: 73 FFs, 496 CLB LUTs
--
-- Compare to:
--
-- * PicoRV32: 483 FFs, 782 LUTs ("small", RV32I only)
-- * FemtoRV32 186 FFs, 411 LUTs ("quark", RV32I only)
-- * SERV: 312 FFs, 182 LUTs (no CSR or timer; RV32I only)
-- * PicoBlaze: 82 FFs, 103 LUTs
--
-- Minimax is competitive, even against RV32I-only cores. When comparing
-- against RV32IC implementations, it does better:
--
-- * SERV: 303 FFs, 336 LUTs (no CSR or timer; RV32IC)
-- * PicoRV32: 518 FFs, 1085 LUTs (RV32IC)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity minimax is
generic (
	constant PC_BITS : natural := 12;
	constant UC_BASE : unsigned(31 downto 0) := x"00000000");
port (
	clk : in std_logic;
	reset : in std_logic;

	-- Program (including microcode) is accessed through here.
	-- Must be two clock cycles' latency. Only microcode may use bits 17..16.
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
	-- Register file
	type reg_array is array(integer range 0 to 63) of std_logic_vector(31 downto 0);
	signal register_file : reg_array := (others => (others => '0'));
	attribute RAM_STYLE : string;
	attribute RAM_STYLE of register_file : signal is "distributed";

	-- Register file address ports
	signal addrS, addrD : std_logic_vector(5 downto 0);
	signal regS, regD, aluA, aluB, aluS, aluX, Dnext : std_logic_vector(31 downto 0); -- datapath
	signal aluZ : std_logic; -- Zero flag

	-- Program counter
	signal pc_fetch, pc_fetch_dly, pc_execute : unsigned(PC_BITS-1 downto 1) := (others => '0');

	-- PC ALU output
	signal aguX, aguA, aguB : unsigned(PC_BITS-1 downto 1) := (others => '0');

	-- Track bubbles and execution inhibits through the pipeline.
	signal bubble, bubble1, bubble2 : std_logic := '1';
	signal caboose : std_logic := '0';
	signal branch_taken, op32_trap, microcode : std_logic := '0';

	-- Writeback and deferred writeback strobes
	signal wb : std_logic := '0';
	signal rs_reg, rd_reg : std_logic_vector(5 downto 0) := (others => '0');
	signal rf_rs1_15 : std_logic := '0';

	-- Strobes for 16-bit instructions
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
	signal op32_lui, dly32_lui : std_logic := '0';
	signal op32_auipc, dly32_auipc : std_logic := '0';
	signal op32_addi, dly32_addi : std_logic := '0';
	signal op32_andi, dly32_andi : std_logic := '0';
	signal op32_ori, dly32_ori : std_logic := '0';
	signal op32_xori, dly32_xori : std_logic := '0';
	signal op32_sll, dly32_sll : std_logic := '0';
	signal op32_srl_sra, dly32_srl_sra : std_logic := '0';
	signal op32_slli, dly32_slli : std_logic := '0';
	signal op32_srli_srai, dly32_srli_srai : std_logic := '0';

	-- Shifts take multiple cycles. The following signals are used to track state.
	signal shift_strobe : std_logic;
	signal shamt : unsigned(4 downto 0) := (others => '0');
	signal shamtZ : std_logic;
	signal dly_srl, dly_sra, dly_sll : std_logic := '0';

begin
	-- From 16.8 (RVC Instruction Set Listings)
	op16_addi4spn	<= inst ?= b"000_-_-----_-----_00" and not (bubble or caboose);
	op16_lw		<= inst ?= b"010_-_-----_-----_00" and not (bubble or caboose);
	op16_sw		<= inst ?= b"110_-_-----_-----_00" and not (bubble or caboose);

	op16_addi	<= inst ?= b"000_-_-----_-----_01" and not (bubble or caboose);
	op16_jal	<= inst ?= b"001_-_-----_-----_01" and not (bubble or caboose);
	op16_li		<= inst ?= b"010_-_-----_-----_01" and not (bubble or caboose);
	op16_addi16sp	<= inst ?= b"011_-_00010_-----_01" and not (bubble or caboose);
	op16_lui	<= inst ?= b"011_-_-----_-----_01" and not (bubble or caboose) and not op16_addi16sp;

	op16_srli	<= inst ?= b"100_0_00---_-----_01" and not (bubble or caboose);
	op16_srai	<= inst ?= b"100_0_01---_-----_01" and not (bubble or caboose);
	op16_andi	<= inst ?= b"100_-_10---_-----_01" and not (bubble or caboose);
	op16_sub	<= inst ?= b"100_-_11---_00---_01" and not (bubble or caboose);
	op16_xor	<= inst ?= b"100_-_11---_01---_01" and not (bubble or caboose);
	op16_or		<= inst ?= b"100_-_11---_10---_01" and not (bubble or caboose);
	op16_and	<= inst ?= b"100_-_11---_11---_01" and not (bubble or caboose);
	op16_j		<= inst ?= b"101_-_-----_-----_01" and not (bubble or caboose);
	op16_beqz	<= inst ?= b"110_-_-----_-----_01" and not (bubble or caboose);
	op16_bnez	<= inst ?= b"111_-_-----_-----_01" and not (bubble or caboose);

	op16_slli	<= inst ?= b"000_0_-----_-----_10" and not (bubble or caboose);
	op16_lwsp	<= inst ?= b"010_-_-----_-----_10" and not (bubble or caboose);
	op16_jr		<= inst ?= b"100_0_-----_00000_10" and not (bubble or caboose);
	op16_mv		<= inst ?= b"100_0_-----_-----_10" and not (bubble or caboose) and not op16_jr;
	op16_ebreak	<= inst ?= b"100_1_00000_00000_10" and not (bubble or caboose);
	op16_jalr	<= inst ?= b"100_1_-----_00000_10" and not (bubble or caboose) and not op16_ebreak;
	op16_add	<= inst ?= b"100_1_-----_-----_10" and not (bubble or caboose) and not op16_jalr and not op16_ebreak;
	op16_swsp	<= inst ?= b"110_-_-----_-----_10" and not (bubble or caboose);

	-- Non-standard extensions to support microcode are permitted in these opcode gaps
	op16_slli_setrd	<= inst ?= b"000_1_-----_00000_10" and not (bubble or caboose) and microcode;
	op16_slli_setrs	<= inst ?= b"000_1_-----_00001_10" and not (bubble or caboose) and microcode;
	op16_slli_thunk	<= inst ?= b"000_1_-----_00010_10" and not (bubble or caboose) and microcode;

	-- Directly implemented RV32I instructions
	op32_lui	<= inst ?= b"-_---_-----_0110111" and not (bubble or caboose);
	op32_addi	<= inst ?= b"-_000_-----_0010011" and not (bubble or caboose);
	op32_andi	<= inst ?= b"-_111_-----_0010011" and not (bubble or caboose);
	op32_ori	<= inst ?= b"-_110_-----_0010011" and not (bubble or caboose);
	op32_xori	<= inst ?= b"-_100_-----_0010011" and not (bubble or caboose);
	op32_slli	<= inst ?= b"-_001_-----_0010011" and not (bubble or caboose);
	op32_srli_srai	<= inst ?= b"-_101_-----_0010011" and not (bubble or caboose);
	op32_auipc	<= inst ?= b"-_---_-----_0010111" and not (bubble or caboose);
	op32_sll	<= inst ?= b"-_001_-----_0110011" and not (bubble or caboose);
	op32_srl_sra	<= inst ?= b"-_101_-----_0110011" and not (bubble or caboose);

	-- Blanket match for RV32I instructions
	op32		<= inst ?= b"---_-_-----_-----_11" and not (bubble or caboose);

	-- Trap condition
	op32_trap <= op32 and not (
		op32_lui or op32_auipc or
		op32_addi or op32_andi or op32_ori or op32_xori or
		op32_slli or op32_srli_srai or
		op32_sll or op32_srl_sra);

	assert UC_BASE(31 downto PC_BITS)=0
		report "Microcode at " & to_hstring(UC_BASE) & " cannot be reached with a " & integer'image(PC_BITS) & "-bit program counter!"
		severity failure;

	-- Data bus outputs tap directly off register/ALU path.
	wdata <= regD;
	addr <= aluX(addr'range);
	rreq <= op16_LWSP or op16_LW;
	wmask <= x"f" and (op16_SWSP or op16_SW);

	-- Instruction bus outputs do too
	inst_addr <= std_logic_vector(pc_fetch & "0");

	-- PC logic
	bubble <= bubble1 or bubble2;

	branch_taken <= (op16_BEQZ and aluZ)
		or (op16_BNEZ and not aluZ)
		or op16_J or op16_JAL or op16_JR or op16_JALR
		or op16_SLLI_THUNK;

	shift_strobe <= op16_srli or op16_slli or op16_srai or
		dly32_sll or dly32_slli or
		dly32_srl_sra or dly32_srli_srai;

	shamtZ <= nor shamt;

	fetch_proc : process(clk)
	begin
		if rising_edge(clk) then
			-- Update fetch instruction unless we're hung up on a multi-cycle instruction word
			if reset='1' then
				pc_fetch <= (others => '0');
			else
				pc_fetch <= aguX;
			end if;

			-- Fetch misses create a 2-cycle penalty
			bubble2 <= reset or branch_taken or op32_trap;

			-- Multi-cycle instructions must correctly pause the fetch/execution pipeline
			bubble1 <= reset or bubble2 or rreq or shift_strobe or dly_srl or dly_sll or dly_sra;

			if shift_strobe or rreq or dly_srl or dly_sll or dly_sra then
				inst_regce <= '0';
			else
				pc_fetch_dly <= pc_fetch;
				pc_execute <= pc_fetch_dly;
				inst_regce <= '1';
			end if;

			caboose <= op32;
			microcode <= (microcode or op32_trap) and not (reset or op16_SLLI_THUNK);

			assert not (microcode and op32_trap)
				report "Double trap!"
				severity failure;

			if reset or op16_SLLI_THUNK then
				microcode <= '0';
			elsif op32_trap then
				microcode <= '1';
			end if;
		end if;
	end process;

	datapath_proc : process(clk)
	begin
		if rising_edge(clk) then
			rd_reg <= (others => '0');
			rs_reg <= (others => '0');

			dly32_lui <= op32_lui;
			dly32_auipc <= op32_auipc;
			dly32_addi <= op32_addi;
			dly32_andi <= op32_andi;
			dly32_ori <= op32_ori;
			dly32_xori <= op32_xori;
			dly32_sll <= op32_sll;
			dly32_srl_sra <= op32_srl_sra;
			dly32_slli <= op32_slli;
			dly32_srli_srai <= op32_srli_srai;

			dly16_lw <= op16_lw;
			dly16_lwsp <= op16_lwsp;
			dly16_slli_setrs <= op16_slli_setrs;
			dly16_slli_setrd <= op16_slli_setrd;

			-- When directly executing RV32I instructions, the lsb
			-- of rs1 shows up in the first instruction half-word
			-- and needs to be kept until the second half-word,
			-- when execution occurs.
			rf_rs1_15 <= inst(15);

			if reset='1' then
				shamt <= (others => '0');
				dly_srl <= '0';
				dly_sll <= '0';
				dly_sra <= '0';
			else
				-- Load and clobber instructions complete a cycle after they are
				-- initiated, so we need to keep some state.
				rd_reg <= (('0' & regD(4 downto 0)) and op16_SLLI_SETRD)
					or (rd_reg and (dly_sll or dly_srl or dly_sra
						or dly32_sll or dly32_srl_sra
						or dly32_slli or dly32_srli_srai))
					or ((microcode & "01" & inst(4 downto 2)) and op16_LW)
					or ((microcode & inst(11 downto 7)) and (op16_LWSP or op32 or op16_SLLI))
					or ((microcode & "01" & inst(9 downto 7)) and (op16_SRLI or op16_SRAI));

				rs_reg <= (('0' & regD(4 downto 0)) and op16_SLLI_SETRS)
					or (rd_reg and (dly_sll or dly_srl or dly_sra)) -- transfer rd_reg -> rs_reg after first shift
					or ((microcode & inst(11 downto 7)) and (op16_LWSP or op16_SLLI))
					or ((microcode & "01" & inst(9 downto 7)) and (op16_SRLI or op16_SRAI))
					or ((microcode & (inst(3 downto 0) & rf_rs1_15)) and (
						dly32_sll or dly32_srl_sra
						or dly32_slli or dly32_srli_srai));

				-- Shift operations also stall the pipeline.
				-- It goes like this:
				--
				-- cycle 0: shift amount transferred into shamt
				-- cycle 1..shamt: shift happens; pipeline otherwise stalled
				if dly_sll or dly_srl or dly_sra then
					if shamtZ then
						dly_srl <= '0';
						dly_sll <= '0';
						dly_sra <= '0';
					else
						shamt <= shamt - 1;
					end if;
				else
					shamt <= (unsigned(inst(6 downto 2)) and (op16_SLLI or op16_SRLI or op16_SRAI))
						or (unsigned(regS(shamt'range)) and (dly32_sll or dly32_srl_sra))
						or (unsigned(inst(8 downto 4)) and (dly32_slli or dly32_srli_srai));

					dly_sll <= op16_SLLI or dly32_sll or dly32_slli;
					dly_srl <= op16_SRLI or ((dly32_srl_sra or dly32_srli_srai) and not inst(14));
					dly_sra <= op16_SRAI or ((dly32_srl_sra or dly32_srli_srai) and inst(14));
				end if;
			end if;
		end if;
	end process;

	-- READ/WRITE register file port
	addrD(4 downto 0) <= rd_reg(4 downto 0)
			or (5ux"01" and (op16_JAL or op16_JALR or op32_trap)) -- write return address into ra
			or (("01" & inst(4 downto 2)) and (op16_ADDI4SPN or op16_SW)) -- data
			or (inst(6 downto 2) and op16_SWSP)
			or (inst(11 downto 7) and (op16_ADDI or op16_ADD
				or (op16_MV and not dly16_slli_setrd)
				or op16_ADDI16SP
				or op16_SLLI_SETRD or op16_SLLI_SETRS
				or op16_LI or op16_LUI
				or op32_LUI or op32_AUIPC))
			or (("01" & inst(9 downto 7)) and (op16_SUB or op16_XOR or op16_OR or op16_AND or op16_ANDI));

	-- READ-ONLY register file port
	addrS(4 downto 0) <= rs_reg(4 downto 0)
			or ((inst(3 downto 0) & rf_rs1_15) and (
				dly32_addi or dly32_andi or dly32_ori or dly32_xori))
			or (5ux"02" and (op16_ADDI4SPN or op16_LWSP or op16_SWSP))
			or (inst(11 downto 7) and (op16_JR or op16_JALR or op16_slli_thunk)) -- jump destination
			or (("01" & inst(9 downto 7)) and (op16_SW or op16_LW or op16_BEQZ or op16_BNEZ))
			or (("01" & inst(4 downto 2)) and (op16_AND or op16_OR or op16_XOR or op16_SUB))
			or (inst(6 downto 2) and ((op16_MV and not dly16_slli_setrs) or op16_ADD))
			or (inst(8 downto 4) and (dly32_sll or dly32_srl_sra)); -- capture shamt

	-- Select between "normal" and "microcode" register banks.
	addrD(5) <= rd_reg(5) or op32_trap or (microcode xor dly16_slli_setrd);
	addrS(5) <= rs_reg(5) or op32_trap or (microcode xor dly16_slli_setrs);

	-- Look up register file contents combinatorially
	regD <= register_file(to_integer(unsigned(addrD)));
	regS <= register_file(to_integer(unsigned(addrS)));

	aluA <= (regD and (op16_ADD or op16_ADDI or op16_SUB
			or op16_AND or op16_ANDI
			or op16_OR or op16_XOR
			or dly_sll or dly_srl or dly_sra
			or dly32_lui
			or dly32_auipc
			or op16_ADDI16SP))
		or ((22x"0" & inst(10 downto 7) & inst(12 downto 11) & inst(5) & inst(6) & "00") and op16_ADDI4SPN)
		or ((std_logic_vector'(31 downto 8 => '0') & inst(8 downto 7) & inst(12 downto 9) & "00") and op16_SWSP)
		or ((std_logic_vector'(31 downto 8 => '0') & inst(3 downto 2) & inst(12) & inst(6 downto 4) & "00") and op16_LWSP)
		or (std_logic_vector(resize(signed(inst(15 downto 4)), 32)) and (
			dly32_addi or dly32_andi or
			dly32_ori or dly32_xori))
		or ((25x"0" & inst(5) & inst(12 downto 10) & inst(6) & "00") and (op16_LW or op16_SW))
		or (std_logic_vector(resize(pc_execute & "0", 32)) and op32_auipc);

	aluB <= regS
		or ((std_logic_vector'(31 downto 5 => inst(12)) & inst(6 downto 2)) and (op16_ADDI or op16_ANDI or op16_LI))
		or ((16x"0000" & inst(15 downto 12) & 12x"000") and (op32_lui or op32_auipc))
		or ((inst & 16x"0000") and (dly32_lui or dly32_auipc))
		or ((std_logic_vector'(31 downto 17 => inst(12)) & inst(6 downto 2) & 12x"0") and op16_LUI)
		or ((std_logic_vector'(31 downto 9 => inst(12)) & inst(4 downto 3) & inst(5) & inst(2) & inst(6) & x"0") and op16_ADDI16SP);

	-- This synthesizes into 4 CARRY8s - no need for manual xor/cin heroics
	aluS <= std_logic_vector(signed(aluA) - signed(aluB)) when op16_SUB
		else std_logic_vector(signed(aluA) + signed(aluB));

	aluZ <= nor aluS;

	aluX <= (aluS and (
			op16_ADD or op16_SUB or op16_ADDI
			or op16_LI or op16_LUI
			or op32_LUI or dly32_LUI
			or op32_AUIPC or dly32_AUIPC
			or op32_ADDI or dly32_ADDI
			or op16_ADDI4SPN or op16_ADDI16SP
			or op16_LW or op16_SW
			or op16_LWSP or op16_SWSP
			or op16_MV)) or
		((aluB(30 downto 0) & "0") and (not shamtZ) and dly_sll) or
		((((dly_SRA or op16_srai) and aluB(31)) & aluB(31 downto 1)) and (not shamtZ) and (dly_sra or dly_srl)) or
		(aluB and (dly_sra or dly_srl or dly_sll) and shamtZ) or
		((aluA and aluB) and (op16_ANDI or op16_AND or dly32_andi)) or
		((aluA xor aluB) and (op16_XOR or dly32_xori)) or
		((aluA or aluB) and (op16_OR or dly32_ori));

	Dnext <= (rdata and (dly16_lw or dly16_lwsp))
		or (std_logic_vector(resize(pc_fetch_dly & "0", 32) and (op16_JAL or op16_JALR or op32_trap))) -- instruction following the jump (hence _dly)
		or aluX(addr'range);

	-- Address Generation Unit (AGU)

	aguA <= (pc_fetch and not (op16_JR or op16_JALR or op32_trap or branch_taken))
		or (pc_execute and branch_taken and not (op16_JR or op16_JALR or op16_slli_thunk));

	aguB <= (unsigned(regS(aguB'range)) and (op16_JR or op16_JALR or op16_slli_thunk))
		or unsigned((std_logic_vector'(aguB'high downto 11 => inst(12)) & inst(8) & inst(10 downto 9) & inst(6) & inst(7) & inst(2) & inst(11) & inst(5 downto 3))
			and branch_taken and (op16_J or op16_JAL))
		or unsigned((std_logic_vector'(aguB'high downto 8 => inst(12)) & inst(6 downto 5) & inst(2) & inst(11 downto 10) & inst(4 downto 3))
			and branch_taken and (op16_BNEZ or op16_BEQZ))
		or (unsigned(UC_BASE(pc_fetch'range)) and op32_trap);

	aguX <= (aguA + aguB) + not (shift_strobe or branch_taken or dly_srl or dly_sra or dly_sll or rreq or op32_trap);

	wb <= op32_trap or			-- writes microcode x1/ra
		dly16_lw or dly16_lwsp or	-- writes data
		op16_JAL or op16_JALR or	-- writes x1/ra
		op16_LI or op16_LUI or
		op16_ADDI or op16_ADDI4SPN or op16_ADDI16SP or
		dly_SRL or dly_SLL or dly_SRA or
		op16_ANDI or op16_MV or op16_ADD or
		op16_AND or op16_OR or op16_XOR or op16_SUB or
		op32_lui or dly32_lui or
		op32_auipc or dly32_auipc or
		dly32_addi or dly32_andi or dly32_ori or dly32_xori;

	regs_proc : process(clk)
	begin
		if rising_edge(clk) then
			-- writeback
			if (or addrD(4 downto 0)) and wb then
				register_file(to_integer(unsigned(addrD))) <= Dnext;
			end if;
		end if;
	end process;

end behav;
