/*
 * Minimax: microcoded RISC-V
 *
 * (c) 2022-2023 Three-Speed Logic, Inc. <gsmecher@threespeedlogic.com>
 * (c) 2022-2023 Sean Cross <sean@xobs.io>
 *
 * RISC-V's compressed instruction (RVC) extension is intended as an add-on to
 * the regular, 32-bit instruction set, not a replacement or competitor. Its
 * designers designed RVC instructions to be expanded into regular 32-bit RV32I
 * equivalents via a pre-decoder.
 *
 * What happens if we *explicitly* architect a RISC-V CPU to execute RVC
 * instructions, and "mop up" any RV32I instructions that aren't convenient via
 * a microcode layer? What architectural optimizations are unlocked as a result?
 *
 * "Minimax" is an experimental RISC-V implementation intended to establish if
 * an RVC-optimized CPU is, in practice, any simpler than an ordinary RV32I
 * core with pre-decoder. While it passes a modest test suite, you should not
 * use it without caution. (There are a large number of excellent, open source,
 * "little" RISC-V implementations you should probably use reach for first.)
 */

`default_nettype none

module minimax (
   input wire clk,
   input wire reset,
   input wire [15:0] inst,
   output wire inst_ce,
   input wire [31:0] rdata,
   output wire[PC_BITS-1:0] inst_addr,
   output reg [31:0] addr,
   output reg [31:0] wdata,
   output reg [3:0] wmask,
   output reg rreq,
   input wire rack);

  parameter PC_BITS = 12;
  parameter [31:0] UC_BASE = 32'h00000000;

  wire [31:0] uc_base;
  assign uc_base = UC_BASE;

  // Register file
  reg [31:0] register_file[63:0];

  // Instruction register
  reg [15:0] inst_e;

  // Register file address ports
  wire [5:0] addrS, addrD;
  reg [4:0] addrD_port, addrS_port;
  reg bD_banksel, bS_banksel;
  wire [31:0] regS, regD, aluA, aluB, aluS, aluX;

  // Program counter
  reg [PC_BITS-1:1] pc_f = {(PC_BITS-1){1'b0}};
  reg [PC_BITS-1:1] pc_d = {(PC_BITS-1){1'b0}};
  reg [PC_BITS-1:1] pc_e = {(PC_BITS-1){1'b0}};

  // PC ALU output
  wire [PC_BITS-1:1] aguX, aguA, aguB;

  // Track bubbles and stalls through the pipeline.
  reg bubble_d=1'b1, bubble_e = 1'b1;
  wire stall_f, stall_d, stall_e;

  reg microcode = 1'b0;

  // Instruction strobes at fetch and decode cycles
  logic op16_add_d, op16_add_e,
    op16_addi_d, op16_addi_e,
    op16_addi16sp_d, op16_addi16sp_e,
    op16_addi4spn_d, op16_addi4spn_e,
    op16_and_d, op16_and_e,
    op16_andi_d, op16_andi_e,
    op16_beqz_d, op16_beqz_e,
    op16_bnez_d, op16_bnez_e,
    op16_ebreak_d, op16_ebreak_e,
    op16_j_d, op16_j_e,
    op16_jal_d, op16_jal_e,
    op16_jalr_d, op16_jalr_e,
    op16_jr_d, op16_jr_e,
    op16_li_d, op16_li_e,
    op16_lui_d, op16_lui_e,
    op16_lw_d, op16_lw_e,
    op16_lwsp_d, op16_lwsp_e,
    op16_mv_d, op16_mv_e,
    op16_or_d, op16_or_e,
    op16_sb_d, op16_sb_e,
    op16_sh_d, op16_sh_e,
    op16_slli_d, op16_slli_e,
    op16_slli_setrd_d, op16_slli_setrd_e,
    op16_slli_setrs_d, op16_slli_setrs_e,
    op16_slli_thunk_d, op16_slli_thunk_e,
    op16_srai_d, op16_srai_e,
    op16_srli_d, op16_srli_e,
    op16_sub_d, op16_sub_e,
    op16_sw_d, op16_sw_e,
    op16_swsp_d, op16_swsp_e,
    op16_xor_d, op16_xor_e,
    op32_d, op32_e;

  // Opcode masks for 16-bit instructions
  wire [15:0] inst_type_masked_d     = inst & 16'b111_0_00000_00000_11;
  wire [15:0] inst_type_masked_zcb_d = inst & 16'b111_1_11000_00000_11;
  wire [15:0] inst_type_masked_i16_d = inst & 16'b111_0_11111_00000_11;
  wire [15:0] inst_type_masked_and_d = inst & 16'b111_0_11000_00000_11;
  wire [15:0] inst_type_masked_op_d  = inst & 16'b111_0_11000_11000_11;
  wire [15:0] inst_type_masked_j_d   = inst & 16'b111_1_00000_11111_11;
  wire [15:0] inst_type_masked_mj_d  = inst & 16'b111_1_00000_00000_11;

  // One-hot decode flags for different instruction types.
  assign op16_addi4spn_d   = (inst_type_masked_d     == 16'b000_0_00000_00000_00);
  assign op16_lw_d         = (inst_type_masked_d     == 16'b010_0_00000_00000_00);
  assign op16_sw_d         = (inst_type_masked_d     == 16'b110_0_00000_00000_00);
  assign op16_sb_d         = (inst_type_masked_zcb_d == 16'b100_0_10000_00000_00);
  assign op16_sh_d         = (inst_type_masked_zcb_d == 16'b100_0_11000_00000_00);

  assign op16_addi_d       = (inst_type_masked_d     == 16'b000_0_00000_00000_01);
  assign op16_jal_d        = (inst_type_masked_d     == 16'b001_0_00000_00000_01);
  assign op16_li_d         = (inst_type_masked_d     == 16'b010_0_00000_00000_01);
  assign op16_addi16sp_d   = (inst_type_masked_i16_d == 16'b011_0_00010_00000_01);
  assign op16_lui_d        = (inst_type_masked_d     == 16'b011_0_00000_00000_01) & ~op16_addi16sp_d;

  assign op16_srli_d       = (inst_type_masked_zcb_d == 16'b100_0_00000_00000_01);
  assign op16_srai_d       = (inst_type_masked_zcb_d == 16'b100_0_01000_00000_01);
  assign op16_andi_d       = (inst_type_masked_and_d == 16'b100_0_10000_00000_01);
  assign op16_sub_d        = (inst_type_masked_op_d  == 16'b100_0_11000_00000_01);
  assign op16_xor_d        = (inst_type_masked_op_d  == 16'b100_0_11000_01000_01);
  assign op16_or_d         = (inst_type_masked_op_d  == 16'b100_0_11000_10000_01);
  assign op16_and_d        = (inst_type_masked_op_d  == 16'b100_0_11000_11000_01);
  assign op16_j_d          = (inst_type_masked_d     == 16'b101_0_00000_00000_01);
  assign op16_beqz_d       = (inst_type_masked_d     == 16'b110_0_00000_00000_01);
  assign op16_bnez_d       = (inst_type_masked_d     == 16'b111_0_00000_00000_01);

  assign op16_slli_d       = (inst_type_masked_mj_d  == 16'b000_0_00000_00000_10);
  assign op16_lwsp_d       = (inst_type_masked_d     == 16'b010_0_00000_00000_10);
  assign op16_jr_d         = (inst_type_masked_j_d   == 16'b100_0_00000_00000_10);
  assign op16_mv_d         = (inst_type_masked_mj_d  == 16'b100_0_00000_00000_10) & ~op16_jr_d;
  assign op16_ebreak_d     = (inst                   == 16'b100_1_00000_00000_10);
  assign op16_jalr_d       = (inst_type_masked_j_d   == 16'b100_1_00000_00000_10) & ~op16_ebreak_d;
  assign op16_add_d        = (inst_type_masked_mj_d  == 16'b100_1_00000_00000_10) & ~op16_jalr_d & ~op16_ebreak_d;
  assign op16_swsp_d       = (inst_type_masked_d     == 16'b110_0_00000_00000_10);

  // Non-standard extensions to support microcode are permitted in these opcode gaps
  assign op16_slli_setrd_d = (inst_type_masked_j_d   == 16'b000_1_00000_00001_10);
  assign op16_slli_setrs_d = (inst_type_masked_j_d   == 16'b000_1_00000_00010_10);
  assign op16_slli_thunk_d = (inst_type_masked_j_d   == 16'b000_1_00000_00100_10);
  assign op32_d = &(inst[1:0]);

  always @(posedge clk) begin
    if(~stall_e) begin
      op16_add_e <= op16_add_d;
      op16_addi_e <= op16_addi_d;
      op16_addi16sp_e <= op16_addi16sp_d;
      op16_addi4spn_e <= op16_addi4spn_d;
      op16_and_e <= op16_and_d;
      op16_andi_e <= op16_andi_d;
      op16_beqz_e <= op16_beqz_d;
      op16_bnez_e <= op16_bnez_d;
      op16_ebreak_e <= op16_ebreak_d;
      op16_j_e <= op16_j_d;
      op16_jal_e <= op16_jal_d;
      op16_jalr_e <= op16_jalr_d;
      op16_jr_e <= op16_jr_d;
      op16_li_e <= op16_li_d;
      op16_lui_e <= op16_lui_d;
      op16_lw_e <= op16_lw_d;
      op16_lwsp_e <= op16_lwsp_d;
      op16_mv_e <= op16_mv_d;
      op16_or_e <= op16_or_d;
      op16_sb_e <= op16_sb_d;
      op16_sh_e <= op16_sh_d;
      op16_slli_e <= op16_slli_d;
      op16_slli_setrd_e <= op16_slli_setrd_d;
      op16_slli_setrs_e <= op16_slli_setrs_d;
      op16_slli_thunk_e <= op16_slli_thunk_d;
      op16_srai_e <= op16_srai_d;
      op16_srli_e <= op16_srli_d;
      op16_sub_e <= op16_sub_d;
      op16_sw_e <= op16_sw_d;
      op16_swsp_e <= op16_swsp_d;
      op16_xor_e <= op16_xor_d;
      op32_e <= op32_d;
    end
  end

  // Stall loads between rreq and rack
  reg ld_stall = 0;

  // Data bus reads and writes are registered
  assign stall_e = ~bubble_e & (op16_lw_e | op16_lwsp_e | ld_stall) & ~rack;
  assign stall_d = stall_e & ~bubble_d;
  assign stall_f = stall_e & ~bubble_d;

  always @(posedge clk) begin
    rreq <= 1'b0;
    addr <= 32'b0;
    wmask <= 4'h0;
    wdata <= 32'b0;

    if(reset | rack) begin
      ld_stall <= 'b0;
    end else if(~bubble_e & ~ld_stall & (op16_lw_e | op16_lwsp_e)) begin
      addr <= aluS;
      ld_stall <= 1'b1;
      rreq <= 1'b1;
    end else if(~bubble_e & (op16_swsp_e | op16_sw_e | op16_sh_e | op16_sb_e)) begin
      addr <= aluS;
      wmask <= {4{op16_swsp_e}} | {4{op16_sw_e}} |
        {4{op16_sh_e}} & {{2{inst_e[5]}}, {2{~inst_e[5]}}} |
        {4{op16_sb_e}} & {inst_e[6:5]==2'b11, inst_e[6:5]==2'b01, inst_e[6:5]==2'b10, inst_e[6:5]==2'b00};
      wdata <= oshift;
    end
  end

  // Instruction bus outputs
  assign inst_addr = {pc_f, 1'b0};
  assign inst_ce = ~stall_f;

  // PC logic
  wire cond_taken = (op16_beqz_e & ~|regS) | (op16_bnez_e & |regS);
  wire branch_taken = cond_taken | op16_j_e | op16_jal_e | op16_jr_e | op16_jalr_e;

  wire dest_abs = ~bubble_e & (op32_e | op16_slli_thunk_e | op16_jr_e | op16_jalr_e);
  wire dest_pcrel = ~bubble_e & (cond_taken | op16_j_e | op16_jal_e);
  wire dest_next = ~dest_abs & ~dest_pcrel & ~stall_e;

  // Fetch Process
  always @(posedge clk) begin

    // Update fetch instruction unless bubbling
    if (reset) begin
      pc_f <= 0;
      pc_d <= 0;
      pc_e <= 0;
      bubble_d <= 1'b1;
      bubble_e <= 1'b1;
      microcode <= 1'b0;
    end else begin
      if(~stall_f)
        pc_f <= aguX;

      // Instruction mis-fetches create a 2-cycle penalty
      if(~stall_d) begin
        pc_d <= pc_f;
        bubble_d <= ~bubble_e & ~dest_next;
      end

      if(~stall_e) begin
        pc_e <= pc_d;
        inst_e <= inst;
        bubble_e <= (~bubble_e & ~dest_next) | bubble_d;

        if(~bubble_e)
          microcode <= (microcode | op32_e) & ~op16_slli_thunk_e;
      end
    end

`ifdef ENABLE_ASSERTS
    if (microcode & op32_e & ~bubble_e) begin
      $display("Double trap!");
      $stop;
    end

    // Check to make sure the microcode doesn't exceed the program counter size
    if (UC_BASE[31:PC_BITS] != 0) begin
      $display("Microcode at 0x%0h cannot be reached with a %d-bit program counter!", UC_BASE, PC_BITS);
      $stop;
    end
`endif

  end

  // READ/WRITE register file port
  reg [31:0] aluA_imm, aluB_imm;
  always @(posedge clk) begin
    if(reset | bubble_d) begin
      addrD_port <= 0;
      addrS_port <= 0;
      bD_banksel <= 0;
      bS_banksel <= 0;
      aluA_imm <= 0;
      aluB_imm <= 0;
    end else if(~stall_e) begin
      addrD_port <=
        (5'b00001 & {5{op16_jal_d | op16_jalr_d | op32_d}}) // write return address into ra
        | (regD[4:0] & {5{op16_slli_setrd_e & ~bubble_e}})
        | (inst[6:2] & {5{op16_swsp_d}})
        | ({2'b01, inst[9:7]} & {5{op16_sub_d | op16_xor_d | op16_or_d | op16_and_d | op16_andi_d | op16_srli_d | op16_srai_d}})
        | ({2'b01, inst[4:2]} & {5{op16_addi4spn_d | op16_sw_d | op16_sh_d | op16_sb_d | op16_lw_d}}) // data
        | (inst[11:7] & ({5{op16_addi_d | op16_add_d
            | (op16_mv_d & (~op16_slli_setrd_e | bubble_e))
            | op16_addi16sp_d
            | op16_slli_setrd_d | op16_slli_setrs_d
            | op16_li_d | op16_lui_d
            | op16_lwsp_d
            | op16_slli_d}}));

      // READ-ONLY register file port
      addrS_port <= (regD[4:0] & {5{op16_slli_setrs_e & ~bubble_e}})
          | (5'b00010 & {5{op16_addi4spn_d | op16_lwsp_d | op16_swsp_d}})
          | ({2'b01, inst[9:7]} & {5{op16_sw_d | op16_sh_d | op16_sb_d | op16_lw_d | op16_beqz_d | op16_bnez_d}})
          | ({2'b01, inst[4:2]} & {5{op16_and_d | op16_or_d | op16_xor_d | op16_sub_d}})
          | (inst[11:7] & {5{op16_jr_d | op16_jalr_d | op16_slli_thunk_d}}) // jump destination
          | (inst[6:2] & {5{(op16_mv_d & (~op16_slli_setrs_e | bubble_e)) | op16_add_d}});

      bD_banksel <= (microcode ^ (~bubble_e & op16_slli_setrd_e)) | op32_d;
      bS_banksel <= (microcode ^ (~bubble_e & op16_slli_setrs_e)) | op32_d;

      unique case(1'b1)
        op16_addi4spn_d: aluA_imm <= {22'b0, inst[10:7], inst[12:11], inst[5], inst[6], 2'b0};
        op16_swsp_d: aluA_imm <= {24'b0, inst[8:7], inst[12:9], 2'b0};
        op16_lwsp_d: aluA_imm <= {24'b0, inst[3:2], inst[12], inst[6:4], 2'b0};
        op16_lw_d | op16_sw_d: aluA_imm <= {25'b0, inst[5], inst[12:10], inst[6], 2'b0};
        default: aluA_imm <= 0;
      endcase

      unique case(1'b1)
        op16_addi_d | op16_andi_d | op16_li_d: aluB_imm <= {{27{inst[12]}}, inst[6:2]};
        op16_lui_d: aluB_imm <= {{15{inst[12]}}, inst[6:2], 12'b0};
        op16_addi16sp_d: aluB_imm <= {{23{inst[12]}}, inst[4:3], inst[5], inst[2], inst[6], 4'b0};
        default: aluB_imm <= 0;
      endcase
    end
  end

  // Select between "normal" and "microcode" register banks.
  assign addrD = {bD_banksel, addrD_port};
  assign addrS = {bS_banksel, addrS_port};

  // Look up register file contents combinatorially
  assign regD = register_file[addrD];
  assign regS = register_file[addrS];

  assign aluA = aluA_imm
    | (regD & {32{op16_add_e | op16_addi_e | op16_sub_e
                | op16_and_e | op16_andi_e
                | op16_or_e | op16_xor_e
                | op16_addi16sp_e}});

  assign aluB = aluB_imm | regS;

  // This synthesizes into 4 CARRY8s - no need for manual xor/cin heroics
  assign aluS = op16_sub_e ? (aluA - aluB) : (aluA + aluB);

  // Full shifter: uses a single shift operator, with bit reversal to handle
  // c.slli, c.srli, and c.srai.
  reg shift_right_e;
  reg [4:0] shamt_e;
  always @(posedge clk) begin
    if(reset | bubble_d) begin
      shamt_e <= 0;
      shift_right_e <= 0;
    end else begin
      unique case(1'b1)
        op16_srli_d: shamt_e <= inst[6:2];
        op16_srai_d: shamt_e <= inst[6:2];
        op16_slli_d: shamt_e <= inst[6:2];
        op16_sb_d: shamt_e <= {inst[5], inst[6], 3'b0};
        op16_sh_d: shamt_e <= {inst[5], 4'b0};
        default: shamt_e <= 0; // sw, swsp, ...
      endcase
      shift_right_e <= op16_srli_d | op16_srai_d;
    end
  end

  wire [31:0] regD_reversed = {<<{regD}};
  wire signed [32:0] ishift = shift_right_e ? {regD[31] & op16_srai_e, regD} : {1'b0, regD_reversed};
  wire [32:0] rshift = ishift >>> shamt_e;
  wire [31:0] rshift_reversed = {<<{rshift[31:0]}};
  wire [31:0] oshift = shift_right_e ? rshift[31:0] : rshift_reversed;

  assign aluX = (aluS & (
                    {32{op16_add_e | op16_sub_e | op16_addi_e
                      | op16_li_e | op16_lui_e
                      | op16_addi4spn_e | op16_addi16sp_e}})) |
          ((aluA & aluB) & {32{op16_andi_e | op16_and_e}}) |
          (oshift & {32{op16_slli_e | op16_srai_e | op16_srli_e}}) |
          ((aluA ^ aluB) & {32{op16_xor_e}}) |
          ((aluA | aluB) & {32{op16_or_e | op16_mv_e}}) |
          (rdata & {32{rack}}) |
          ({{(32-PC_BITS){1'b0}}, pc_d[PC_BITS-1:1], 1'b0} & {32{op16_jal_e | op16_jalr_e | op32_e}}); //  instruction following the jump

  // Address Generation Unit (AGU)
  assign aguA = (pc_f & {(PC_BITS-1){dest_next}})
        | (pc_e & {(PC_BITS-1){dest_pcrel}});

  assign aguB = (regS[PC_BITS-1:1] & {(PC_BITS-1){~bubble_e & dest_abs}})
        | ({{(PC_BITS-11){inst_e[12]}}, inst_e[8], inst_e[10:9], inst_e[6], inst_e[7], inst_e[2], inst_e[11], inst_e[5:3]}
              & {(PC_BITS-1){~bubble_e & (op16_j_e | op16_jal_e)}})
        | ({{(PC_BITS-8){inst_e[12]}}, inst_e[6:5], inst_e[2], inst_e[11:10], inst_e[4:3]}
              & {(PC_BITS-1){~bubble_e & cond_taken}})
        | (uc_base[PC_BITS-1:1] & {(PC_BITS-1){~bubble_e & op32_e}})
        | {{(PC_BITS-2){1'b0}}, dest_next};

  assign aguX = aguA + aguB;

  // Register file
  reg wb;
  always @(posedge clk) begin
    wb <= ~reset & (
      op32_d |
      op16_jal_d | op16_jalr_d |   // writes x1/ra
      op16_li_d | op16_lui_d |
      op16_addi_d | op16_addi4spn_d | op16_addi16sp_d |
      op16_andi_d | op16_mv_d | op16_add_d |
      op16_and_d | op16_or_d | op16_xor_d | op16_sub_d |
      op16_slli_d | op16_srli_d | op16_srai_d);

    // writeback
    if (|(addrD[4:0]) & ~(stall_e | bubble_e) & (wb | rack)) begin
      register_file[addrD] <= aluX;
    end
  end

  // Tracing
`ifdef ENABLE_TRACE
  initial begin
      $display(
          "PC_FETCH|"
        , "  PC_DECODE    |"
        , "  PC_EXEC      |"
        , "   aguA |"
        , "   aguB |"
        , "   aguX |"
        , " OPCODE |"
        , "addrD|"
        , "addrS|"
        , "  regD  |"
        , "  regS  |"
        , "  aluA  |"
        , "  aluB  |"
        , "  aluS  |"
        , "  aluX  |"
        , "FLAGS");
  end

  // This register can be viewed in the resulting VCD file by setting
  // the display type to "ASCII".
  reg [8*8-1:0] opcode;

  always @(posedge clk) begin
      $write("%8H ", {pc_f, 1'b0});
      $write("%8H ", {pc_d, 1'b0});
      $write("(%4H) ", inst);
      $write("%8H ", {pc_e, 1'b0});
      $write("(%4H) ", inst_e);
      $write("%8H ", {aguA, 1'b0});
      $write("%8H ", {aguB, 1'b0});
      $write("%8H ", {aguX, 1'b0});

      if(bubble_e)               begin $write("BUBBLE  "); opcode = "BUBBLE  "; end
      else if(op16_addi4spn_e)   begin $write("ADDI4SPN"); opcode = "ADDI4SPN"; end
      else if(op16_lw_e)         begin $write("LW      "); opcode = "LW      "; end
      else if(op16_sw_e)         begin $write("SW      "); opcode = "SW      "; end
      else if(op16_sb_e)         begin $write("SB      "); opcode = "SB      "; end
      else if(op16_sh_e)         begin $write("SH      "); opcode = "SH      "; end
      else if(op16_addi_e)       begin $write("ADDI    "); opcode = "ADDI    "; end
      else if(op16_jal_e)        begin $write("JAL     "); opcode = "JAL     "; end
      else if(op16_li_e)         begin $write("LI      "); opcode = "LI      "; end
      else if(op16_addi16sp_e)   begin $write("ADDI16SP"); opcode = "ADDI16SP"; end
      else if(op16_lui_e)        begin $write("LUI     "); opcode = "LUI     "; end
      else if(op16_srli_e)       begin $write("SRLI    "); opcode = "SRLI    "; end
      else if(op16_srai_e)       begin $write("SRAI    "); opcode = "SRAI    "; end
      else if(op16_andi_e)       begin $write("ANDI    "); opcode = "ANDI    "; end
      else if(op16_sub_e)        begin $write("SUB     "); opcode = "SUB     "; end
      else if(op16_xor_e)        begin $write("XOR     "); opcode = "XOR     "; end
      else if(op16_or_e)         begin $write("OR      "); opcode = "OR      "; end
      else if(op16_and_e)        begin $write("AND     "); opcode = "AND     "; end
      else if(op16_j_e)          begin $write("J       "); opcode = "J       "; end
      else if(op16_beqz_e)       begin $write("BEQZ    "); opcode = "BEQZ    "; end
      else if(op16_bnez_e)       begin $write("BNEZ    "); opcode = "BNEZ    "; end
      else if(op16_slli_e)       begin $write("SLLI    "); opcode = "SLLI    "; end
      else if(op16_lwsp_e)       begin $write("LWSP    "); opcode = "LWSP    "; end
      else if(op16_jr_e)         begin $write("JR      "); opcode = "JR      "; end
      else if(op16_mv_e)         begin $write("MV      "); opcode = "MV      "; end
      else if(op16_ebreak_e)     begin $write("EBREAK  "); opcode = "EBREAK  "; end
      else if(op16_jalr_e)       begin $write("JALR    "); opcode = "JALR    "; end
      else if(op16_add_e)        begin $write("ADD     "); opcode = "ADD     "; end
      else if(op16_swsp_e)       begin $write("SWSP    "); opcode = "SWSP    "; end
      else if(op16_slli_thunk_e) begin $write("THUNK   "); opcode = "THUNK   "; end
      else if(op16_slli_setrd_e) begin $write("SETRD   "); opcode = "SETRD   "; end
      else if(op16_slli_setrs_e) begin $write("SETRS   "); opcode = "SETRS   "; end
      else if(op32_e)            begin $write("RV32I   "); opcode = "RV32I   "; end
      else                       begin $write("NOP?    "); opcode = "NOP?    "; end

      $write("  %1b.%2H", addrD[5], addrD[4:0]);
      $write("  %1b.%2H", addrS[5], addrS[4:0]);

      $write(" %8H", regD);
      $write(" %8H", regS);

      $write(" %8H", aluA);
      $write(" %8H", aluB);
      $write(" %8H", aluS);
      $write(" %8H", aluX);

      if(op32_e) begin
        $write(" TRAP");
      end
      if(cond_taken) begin
        $write(" TAKEN");
      end

      if(bubble_d | bubble_e) begin
        $write (" B");
        if(bubble_d) $write("D");
        if(bubble_e) $write("E");
      end

      if(stall_f | stall_d | stall_e) begin
        $write (" S");
        if(stall_f) $write("F");
        if(stall_d) $write("D");
        if(stall_e) $write("E");
      end

      if(dest_abs) $write(" DA");
      if(dest_next) $write(" DN");
      if(dest_pcrel) $write(" DP");

      if(wb) $write(" WB");
      if(reset) $write(" RESET");
      if(microcode) $write(" MCODE");
      if(| wmask) begin
        $write(" WMASK=%0h", wmask);
        $write(" ADDR=%0h", addr);
        $write(" WDATA=%0h", wdata);
      end
      if(rreq) begin
        $write(" RREQ");
        $write(" ADDR=%0h", addr);
      end
      if(rack) $write(" RACK");

      $display("");
    end
`endif // `ifdef ENABLE_TRACE

  initial begin
    for(integer i=0; i<64; i++) begin
      register_file[i] = 32'h00000000;
    end
  end

`ifdef ENABLE_REGISTER_INSPECTION
  // Wires that make it easier to inspect the register file during simulation
  wire [31:0] cpu_x0 = register_file[0];
  wire [31:0] cpu_x1 = register_file[1];
  wire [31:0] cpu_x2 = register_file[2];
  wire [31:0] cpu_x3 = register_file[3];
  wire [31:0] cpu_x4 = register_file[4];
  wire [31:0] cpu_x5 = register_file[5];
  wire [31:0] cpu_x6 = register_file[6];
  wire [31:0] cpu_x7 = register_file[7];
  wire [31:0] cpu_x8 = register_file[8];
  wire [31:0] cpu_x9 = register_file[9];
  wire [31:0] cpu_x10 = register_file[10];
  wire [31:0] cpu_x11 = register_file[11];
  wire [31:0] cpu_x12 = register_file[12];
  wire [31:0] cpu_x13 = register_file[13];
  wire [31:0] cpu_x14 = register_file[14];
  wire [31:0] cpu_x15 = register_file[15];
  wire [31:0] cpu_x16 = register_file[16];
  wire [31:0] cpu_x17 = register_file[17];
  wire [31:0] cpu_x18 = register_file[18];
  wire [31:0] cpu_x19 = register_file[19];
  wire [31:0] cpu_x20 = register_file[20];
  wire [31:0] cpu_x21 = register_file[21];
  wire [31:0] cpu_x22 = register_file[22];
  wire [31:0] cpu_x23 = register_file[23];
  wire [31:0] cpu_x24 = register_file[24];
  wire [31:0] cpu_x25 = register_file[25];
  wire [31:0] cpu_x26 = register_file[26];
  wire [31:0] cpu_x27 = register_file[27];
  wire [31:0] cpu_x28 = register_file[28];
  wire [31:0] cpu_x29 = register_file[29];
  wire [31:0] cpu_x30 = register_file[30];
  wire [31:0] cpu_x31 = register_file[31];

  wire [31:0] uc_x0 = register_file[0 + 32];
  wire [31:0] uc_x1 = register_file[1 + 32];
  wire [31:0] uc_x2 = register_file[2 + 32];
  wire [31:0] uc_x3 = register_file[3 + 32];
  wire [31:0] uc_x4 = register_file[4 + 32];
  wire [31:0] uc_x5 = register_file[5 + 32];
  wire [31:0] uc_x6 = register_file[6 + 32];
  wire [31:0] uc_x7 = register_file[7 + 32];
  wire [31:0] uc_x8 = register_file[8 + 32];
  wire [31:0] uc_x9 = register_file[9 + 32];
  wire [31:0] uc_x10 = register_file[10 + 32];
  wire [31:0] uc_x11 = register_file[11 + 32];
  wire [31:0] uc_x12 = register_file[12 + 32];
  wire [31:0] uc_x13 = register_file[13 + 32];
  wire [31:0] uc_x14 = register_file[14 + 32];
  wire [31:0] uc_x15 = register_file[15 + 32];
  wire [31:0] uc_x16 = register_file[16 + 32];
  wire [31:0] uc_x17 = register_file[17 + 32];
  wire [31:0] uc_x18 = register_file[18 + 32];
  wire [31:0] uc_x19 = register_file[19 + 32];
  wire [31:0] uc_x20 = register_file[20 + 32];
  wire [31:0] uc_x21 = register_file[21 + 32];
  wire [31:0] uc_x22 = register_file[22 + 32];
  wire [31:0] uc_x23 = register_file[23 + 32];
  wire [31:0] uc_x24 = register_file[24 + 32];
  wire [31:0] uc_x25 = register_file[25 + 32];
  wire [31:0] uc_x26 = register_file[26 + 32];
  wire [31:0] uc_x27 = register_file[27 + 32];
  wire [31:0] uc_x28 = register_file[28 + 32];
  wire [31:0] uc_x29 = register_file[29 + 32];
  wire [31:0] uc_x30 = register_file[30 + 32];
  wire [31:0] uc_x31 = register_file[31 + 32];
`endif // `ifdef ENABLE_REGISTER_INSPECTION

endmodule
