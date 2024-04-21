module minimax (
  input  clk,
   input  reset,
   input  [15:0] inst,
   input  [31:0] rdata,
   output [11:0] inst_addr,
   output inst_regce,
   output [31:0] addr,
   output [31:0] wdata,
   output [3:0] wmask,module minimax (
    input clk,
    input reset,
    input [15:0] inst,
    input [31:0] rdata,
    output reg [11:0] inst_addr,
    output reg inst_regce,
    output reg [31:0] addr,
    output reg [31:0] wdata,
    output reg [3:0] wmask,
    output reg rreq
);

    // Local parameters for configuration
    localparam ADDR_BITS = 11;  // Example parameter

    // Register and wire definitions
    reg [ADDR_BITS-1:0] pc = 0;
    wire [31:0] alu_result;
    wire branch_taken;

    // ALU logic (simplified example)
    assign alu_result = (inst[3:0] == 4'b0101) ? rdata + inst[15:0] : rdata - inst[15:0];

    // Program counter update logic
    always @(posedge clk) begin
        if (!reset) begin
            pc <= pc + (branch_taken ? 2 : 1);  // Simplified branch logic
        end else begin
            pc <= 0;
        end
    end

    // Output logic
    always @(posedge clk) begin
        if (!reset) begin
            inst_addr <= pc;
            addr <= alu_result[11:0];  // Example address computation
            wdata <= rdata;            // Data to write
            wmask <= 4'b1111;          // Write enable all
            rreq <= 1'b1;              // Read request active
            inst_regce <= 1'b1;        // Instruction register clock enable
        end else begin
            inst_addr <= 0;
            addr <= 0;
            wdata <= 0;
            wmask <= 0;
            rreq <= 0;
            inst_regce <= 0;
        end
    end

    // Branch logic example (simplified)
    assign branch_taken = (inst[15:12] == 4'b1100) && (rdata[0] == 1'b1);  // Conditional branch

endmodule

   output rreq);
  wire [5:0] addrs;
  wire [5:0] addrd;
  wire [31:0] regs;
  wire [31:0] regd;
  wire [31:0] alua;
  wire [31:0] alub;
  wire [31:0] alus;
  wire [31:0] alux;
  reg [10:0] pc_fetch;
  reg [10:0] pc_fetch_dly;
  reg [10:0] pc_execute;
  reg [10:0] agux;
  reg [10:0] agua;
  reg [10:0] agub;
  reg bubble;
  reg bubble1;
  reg bubble2;
  reg branch_taken;
  reg microcode;
  reg trap;
  reg op16_trap;
  reg op32_trap;
  reg wb;
  reg [4:0] dra;
  reg op16;
  reg op16_addi4spn;
  reg op16_lw;
  reg dly16_lw;
  reg op16_sw;
  reg op16_addi;
  reg op16_jal;
  reg op16_li;
  reg op16_addi16sp;
  reg op16_lui;
  reg op16_srli;
  reg op16_srai;
  reg op16_andi;
  reg op16_sub;
  reg op16_xor;
  reg op16_or;
  reg op16_and;
  reg op16_j;
  reg op16_beqz;
  reg op16_bnez;
  reg op16_slli;
  reg op16_lwsp;
  reg dly16_lwsp;
  reg op16_jr;
  reg op16_mv;
  reg op16_ebreak;
  reg op16_jalr;
  reg op16_add;
  reg op16_swsp;
  reg op16_slli_setrd;
  reg dly16_slli_setrd;
  reg op16_slli_setrs;
  reg dly16_slli_setrs;
  reg op16_slli_thunk;
  reg op32;
  wire [15:0] n65_o;
  wire n66_o;
  wire n67_o;
  wire n68_o;
  wire [15:0] n71_o;
  wire n72_o;
  wire n73_o;
  wire n74_o;
  wire [15:0] n77_o;
  wire n78_o;
  wire n79_o;
  wire n80_o;
  wire [15:0] n83_o;
  wire n84_o;
  wire n85_o;
  wire n86_o;
  wire [15:0] n89_o;
  wire n90_o;
  wire n91_o;
  wire n92_o;
  wire [15:0] n95_o;
  wire n96_o;
  wire n97_o;
  wire n98_o;
  wire [15:0] n101_o;
  wire n102_o;
  wire n103_o;
  wire n104_o;
  wire [15:0] n107_o;
  wire n108_o;
  wire n109_o;
  wire n110_o;
  wire n111_o;
  wire n112_o;
  wire [15:0] n115_o;
  wire n116_o;
  wire n117_o;
  wire n118_o;
  wire [15:0] n121_o;
  wire n122_o;
  wire n123_o;
  wire n124_o;
  wire [15:0] n127_o;
  wire n128_o;
  wire n129_o;
  wire n130_o;
  wire [15:0] n133_o;
  wire n134_o;
  wire n135_o;
  wire n136_o;
  wire [15:0] n139_o;
  wire n140_o;
  wire n141_o;
  wire n142_o;
  wire [15:0] n145_o;
  wire n146_o;
  wire n147_o;
  wire n148_o;
  wire [15:0] n151_o;
  wire n152_o;
  wire n153_o;
  wire n154_o;
  wire [15:0] n157_o;
  wire n158_o;
  wire n159_o;
  wire n160_o;
  wire [15:0] n163_o;
  wire n164_o;
  wire n165_o;
  wire n166_o;
  wire [15:0] n169_o;
  wire n170_o;
  wire n171_o;
  wire n172_o;
  wire [15:0] n175_o;
  wire n176_o;
  wire n177_o;
  wire n178_o;
  wire [15:0] n181_o;
  wire n182_o;
  wire n183_o;
  wire n184_o;
  wire [15:0] n187_o;
  wire n188_o;
  wire n189_o;
  wire n190_o;
  wire [15:0] n193_o;
  wire n194_o;
  wire n195_o;
  wire n196_o;
  wire n197_o;
  wire n198_o;
  wire [15:0] n201_o;
  wire n202_o;
  wire n203_o;
  wire n204_o;
  wire [15:0] n207_o;
  wire n208_o;
  wire n209_o;
  wire n210_o;
  wire n211_o;
  wire n212_o;
  wire [15:0] n215_o;
  wire n216_o;
  wire n217_o;
  wire n218_o;
  wire n219_o;
  wire n220_o;
  wire n221_o;
  wire n222_o;
  wire [15:0] n225_o;
  wire n226_o;
  wire n227_o;
  wire n228_o;
  wire [15:0] n231_o;
  wire n232_o;
  wire n233_o;
  wire n234_o;
  wire [15:0] n237_o;
  wire n238_o;
  wire n239_o;
  wire n240_o;
  wire [15:0] n243_o;
  wire n244_o;
  wire n245_o;
  wire n246_o;
  wire [1:0] n247_o;
  wire n248_o;
  wire n249_o;
  wire n250_o;
  wire [1:0] n251_o;
  wire n252_o;
  wire n253_o;
  wire n254_o;
  wire n255_o;
  wire n256_o;
  wire n257_o;
  wire n258_o;
  wire n259_o;
  wire n260_o;
  wire n261_o;
  wire n262_o;
  wire n263_o;
  wire n264_o;
  wire n265_o;
  wire n266_o;
  wire n267_o;
  wire n268_o;
  wire n269_o;
  wire n270_o;
  wire n271_o;
  wire n272_o;
  wire n273_o;
  wire n274_o;
  wire n275_o;
  wire n276_o;
  wire n277_o;
  wire n278_o;
  wire n279_o;
  wire n280_o;
  wire n281_o;
  wire n282_o;
  wire n283_o;
  wire n284_o;
  wire n285_o;
  wire n286_o;
  wire n287_o;
  wire n288_o;
  wire [3:0] n290_o;
  wire [3:0] n291_o;
  wire [11:0] n293_o;
  wire n294_o;
  wire n295_o;
  wire n296_o;
  wire n297_o;
  wire n298_o;
  wire n299_o;
  wire n300_o;
  wire n301_o;
  wire n302_o;
  wire n303_o;
  wire n304_o;
  wire n305_o;
  wire n308_o;
  wire [10:0] n309_o;
  wire [10:0] n310_o;
  wire n311_o;
  wire n312_o;
  wire n313_o;
  wire n314_o;
  wire n317_o;
  wire [10:0] n318_o;
  wire [10:0] n319_o;
  wire n320_o;
  wire n321_o;
  wire n322_o;
  wire n323_o;
  wire n324_o;
  wire n325_o;
  wire n327_o;
  wire n328_o;
  reg n343_q;
  wire [4:0] n346_o;
  wire n347_o;
  wire [4:0] n348_o;
  wire [4:0] n349_o;
  wire [2:0] n350_o;
  wire [4:0] n352_o;
  wire [4:0] n353_o;
  wire [4:0] n354_o;
  wire [4:0] n355_o;
  wire [4:0] n356_o;
  wire n357_o;
  wire [4:0] n358_o;
  wire [4:0] n359_o;
  wire [4:0] n360_o;
  wire n367_o;
  wire n368_o;
  wire [4:0] n369_o;
  wire [4:0] n370_o;
  wire n371_o;
  wire n372_o;
  wire [4:0] n374_o;
  wire [4:0] n375_o;
  wire [4:0] n376_o;
  wire [2:0] n377_o;
  wire [4:0] n379_o;
  wire n380_o;
  wire [4:0] n381_o;
  wire [4:0] n382_o;
  wire [4:0] n383_o;
  wire [4:0] n384_o;
  wire [4:0] n385_o;
  wire [4:0] n386_o;
  wire [4:0] n387_o;
  wire [4:0] n388_o;
  wire n389_o;
  wire n390_o;
  wire n391_o;
  wire n392_o;
  wire n393_o;
  wire n394_o;
  wire n395_o;
  wire n396_o;
  wire n397_o;
  wire n398_o;
  wire [4:0] n399_o;
  wire [4:0] n400_o;
  wire [4:0] n401_o;
  wire [2:0] n402_o;
  wire [4:0] n404_o;
  wire n405_o;
  wire n406_o;
  wire n407_o;
  wire n408_o;
  wire n409_o;
  wire n410_o;
  wire [4:0] n411_o;
  wire [4:0] n412_o;
  wire [4:0] n413_o;
  wire [4:0] n414_o;
  wire [4:0] n415_o;
  wire n416_o;
  wire n417_o;
  wire [4:0] n419_o;
  wire [4:0] n420_o;
  wire [4:0] n421_o;
  wire [4:0] n422_o;
  wire n423_o;
  wire n424_o;
  wire n425_o;
  wire [4:0] n426_o;
  wire [4:0] n427_o;
  wire [4:0] n428_o;
  wire [2:0] n429_o;
  wire [4:0] n431_o;
  wire n432_o;
  wire n433_o;
  wire n434_o;
  wire [4:0] n435_o;
  wire [4:0] n436_o;
  wire [4:0] n437_o;
  wire [2:0] n438_o;
  wire [4:0] n440_o;
  wire n441_o;
  wire n442_o;
  wire n443_o;
  wire [4:0] n444_o;
  wire [4:0] n445_o;
  wire [4:0] n446_o;
  wire [4:0] n447_o;
  wire n448_o;
  wire n449_o;
  wire n450_o;
  wire [4:0] n451_o;
  wire [4:0] n452_o;
  wire [4:0] addrS_4_0;
  wire n454_o;
  wire n455_o;
  wire microcode_xor_dly16_slli_setrs;
  wire addrS_5;
  wire n468_o;
  wire n469_o;
  wire n470_o;
  wire n471_o;
  wire n472_o;
  wire n473_o;
  wire n474_o;
  wire n475_o;
  wire n476_o;
  wire n477_o;
  wire [31:0] n478_o;
  wire [31:0] n479_o;
  wire [3:0] n480_o;
  wire [25:0] n482_o;
  wire [1:0] n483_o;
  wire [27:0] n484_o;
  wire n485_o;
  wire [28:0] n486_o;
  wire n487_o;
  wire [29:0] n488_o;
  wire [31:0] n490_o;
  wire [31:0] n491_o;
  wire [31:0] n492_o;
  wire [31:0] n493_o;
  wire [1:0] n494_o;
  wire [25:0] n496_o;
  wire [3:0] n497_o;
  wire [29:0] n498_o;
  wire [31:0] n500_o;
  wire [31:0] n501_o;
  wire [31:0] n502_o;
  wire [31:0] n503_o;
  wire [1:0] n504_o;
  wire [25:0] n506_o;
  wire n507_o;
  wire [26:0] n508_o;
  wire [2:0] n509_o;
  wire [29:0] n510_o;
  wire [31:0] n512_o;
  wire [31:0] n513_o;
  wire [31:0] n514_o;
  wire [31:0] n515_o;
  wire n516_o;
  wire [25:0] n518_o;
  wire [2:0] n519_o;
  wire [28:0] n520_o;
  wire n521_o;
  wire [29:0] n522_o;
  wire [31:0] n524_o;
  wire n525_o;
  wire [31:0] n526_o;
  wire [31:0] n527_o;
  wire [31:0] n528_o;
  wire n529_o;
  wire n530_o;
  wire n531_o;
  wire n532_o;
  wire n533_o;
  wire n534_o;
  wire n535_o;
  wire n536_o;
  wire n537_o;
  wire n538_o;
  wire n539_o;
  wire n540_o;
  wire n541_o;
  wire n542_o;
  wire n543_o;
  wire n544_o;
  wire n545_o;
  wire n546_o;
  wire n547_o;
  wire n548_o;
  wire n549_o;
  wire n550_o;
  wire n551_o;
  wire n552_o;
  wire n553_o;
  wire n554_o;
  wire n555_o;
  wire [3:0] n556_o;
  wire [3:0] n557_o;
  wire [3:0] n558_o;
  wire [3:0] n559_o;
  wire [3:0] n560_o;
  wire [3:0] n561_o;
  wire [2:0] n562_o;
  wire [15:0] n563_o;
  wire [10:0] n564_o;
  wire [26:0] n565_o;
  wire [4:0] n566_o;
  wire [31:0] n567_o;
  wire n568_o;
  wire n569_o;
  wire [31:0] n570_o;
  wire [31:0] n571_o;
  wire [31:0] n572_o;
  wire n573_o;
  wire n574_o;
  wire n575_o;
  wire n576_o;
  wire n577_o;
  wire n578_o;
  wire n579_o;
  wire n580_o;
  wire n581_o;
  wire n582_o;
  wire n583_o;
  wire n584_o;
  wire n585_o;
  wire n586_o;
  wire n587_o;
  wire [3:0] n588_o;
  wire [3:0] n589_o;
  wire [3:0] n590_o;
  wire [2:0] n591_o;
  wire [14:0] n592_o;
  wire [4:0] n593_o;
  wire [19:0] n594_o;
  wire [31:0] n596_o;
  wire [31:0] n597_o;
  wire [31:0] n598_o;
  wire [31:0] n599_o;
  wire n600_o;
  wire n601_o;
  wire n602_o;
  wire n603_o;
  wire n604_o;
  wire n605_o;
  wire n606_o;
  wire n607_o;
  wire n608_o;
  wire n609_o;
  wire n610_o;
  wire n611_o;
  wire n612_o;
  wire n613_o;
  wire n614_o;
  wire n615_o;
  wire n616_o;
  wire n617_o;
  wire n618_o;
  wire n619_o;
  wire n620_o;
  wire n621_o;
  wire n622_o;
  wire [3:0] n623_o;
  wire [3:0] n624_o;
  wire [3:0] n625_o;
  wire [3:0] n626_o;
  wire [3:0] n627_o;
  wire [2:0] n628_o;
  wire [15:0] n629_o;
  wire [6:0] n630_o;
  wire [22:0] n631_o;
  wire [1:0] n632_o;
  wire [24:0] n633_o;
  wire n634_o;
  wire [25:0] n635_o;
  wire n636_o;
  wire [26:0] n637_o;
  wire n638_o;
  wire [27:0] n639_o;
  wire [31:0] n641_o;
  wire [31:0] n642_o;
  wire [31:0] n643_o;
  wire [31:0] n644_o;
  wire [31:0] n645_o;
  wire [31:0] n646_o;
  wire [31:0] n647_o;
  wire n648_o;
  wire n649_o;
  wire n650_o;
  wire n651_o;
  wire n652_o;
  wire n653_o;
  wire n654_o;
  wire [31:0] n655_o;
  wire [31:0] n656_o;
  wire n657_o;
  wire n658_o;
  wire [30:0] n659_o;
  wire [31:0] n660_o;
  wire n661_o;
  wire [31:0] n662_o;
  wire [31:0] n663_o;
  wire [31:0] n664_o;
  wire [31:0] n665_o;
  wire n666_o;
  wire [31:0] n667_o;
  wire [31:0] n668_o;
  wire [31:0] n669_o;
  wire [31:0] n670_o;
  wire [31:0] n671_o;
  wire [31:0] n672_o;
  wire [31:0] n673_o;
  wire [31:0] n674_o;
  wire n675_o;
  wire [31:0] n676_o;
  wire [31:0] n677_o;
  wire [31:0] n678_o;
  wire n679_o;
  wire [31:0] n680_o;
  wire [31:0] n681_o;
  wire [31:0] n682_o;
  wire [11:0] n684_o;
  wire [31:0] n685_o;
  wire n686_o;
  wire n687_o;
  wire [31:0] n688_o;
  wire [31:0] n689_o;
  wire [31:0] n690_o;
  wire n691_o;
  wire n692_o;
  wire [10:0] n693_o;
  wire [10:0] n694_o;
  wire [10:0] n695_o;
  wire [10:0] n696_o;
  wire n697_o;
  wire n698_o;
  wire n699_o;
  wire [10:0] n700_o;
  wire [10:0] n701_o;
  wire [10:0] n702_o;
  wire [10:0] n703_o;
  wire n704_o;
  wire n705_o;
  wire [10:0] n706_o;
  wire [10:0] n707_o;
  wire n708_o;
  wire n709_o;
  wire [1:0] n710_o;
  wire [1:0] n711_o;
  wire [3:0] n712_o;
  wire n713_o;
  wire [4:0] n714_o;
  wire n715_o;
  wire [5:0] n716_o;
  wire n717_o;
  wire [6:0] n718_o;
  wire n719_o;
  wire [7:0] n720_o;
  wire [2:0] n721_o;
  wire [10:0] n722_o;
  wire [10:0] n723_o;
  wire [10:0] n724_o;
  wire n725_o;
  wire [10:0] n726_o;
  wire [10:0] n727_o;
  wire [10:0] n728_o;
  wire n729_o;
  wire n730_o;
  wire n731_o;
  wire n732_o;
  wire [3:0] n733_o;
  wire [1:0] n734_o;
  wire [5:0] n735_o;
  wire n736_o;
  wire [6:0] n737_o;
  wire [1:0] n738_o;
  wire [8:0] n739_o;
  wire [1:0] n740_o;
  wire [10:0] n741_o;
  wire [10:0] n742_o;
  wire [10:0] n743_o;
  wire n744_o;
  wire [10:0] n745_o;
  wire [10:0] n746_o;
  wire [10:0] n747_o;
  wire [10:0] n749_o;
  wire [10:0] n750_o;
  wire [10:0] n751_o;
  wire [10:0] n752_o;
  wire n753_o;
  wire n754_o;
  wire n755_o;
  wire [10:0] n756_o;
  wire [10:0] n757_o;
  wire n758_o;
  wire n759_o;
  wire n760_o;
  wire n761_o;
  wire n762_o;
  wire n763_o;
  wire n764_o;
  wire n765_o;
  wire n766_o;
  wire n767_o;
  wire n768_o;
  wire n769_o;
  wire n770_o;
  wire n771_o;
  wire n772_o;
  wire n773_o;
  wire n774_o;
  wire n775_o;
  wire n776_o;
  wire [4:0] n779_o;
  wire n780_o;
  wire n781_o;
  wire [5:0] addrS_wire;
  wire [5:0] n793_o;
  reg [10:0] n794_q;
  reg [10:0] n795_q;
  reg [10:0] n796_q;
  reg n797_q;
  reg n798_q;
  reg n799_q;
  reg [4:0] n800_q;
  reg n801_q;
  reg n802_q;
  reg n803_q;
  reg n804_q;
  reg n805_q;
  wire [31:0] n807_data; // mem_rd
  wire [31:0] n808_data; // mem_rd
  assign inst_addr = n293_o;
  assign inst_regce = n805_q;
  assign addr = alus;
  assign wdata = regd;
  assign wmask = n291_o;
  assign rreq = n287_o;
  /* .\minimax.vhd:57:16  */
  assign addrs = addrS_wire; // (signal)
  /* .\minimax.vhd:57:23  */
  assign addrd = n793_o; // (signal)
  /* .\minimax.vhd:58:16  */
  assign regs = n807_data; // (signal)
  /* .\minimax.vhd:58:22  */
  assign regd = n808_data; // (signal)
  /* .\minimax.vhd:58:28  */
  assign alua = n528_o; // (signal)
  /* .\minimax.vhd:58:34  */
  assign alub = n644_o; // (signal)
  /* .\minimax.vhd:174:21  */
  assign alus = n646_o; // (signal)
  /* .\minimax.vhd:58:46  */
  assign alux = n690_o; // (signal)
  /* .\minimax.vhd:61:16  */
  always @*
    pc_fetch = n794_q; // (isignal)
  initial
    pc_fetch = 11'b00000000000;
  /* .\minimax.vhd:61:26  */
  always @*
    pc_fetch_dly = n795_q; // (isignal)
  initial
    pc_fetch_dly = 11'b00000000000;
  /* .\minimax.vhd:61:40  */
  always @*
    pc_execute = n796_q; // (isignal)
  initial
    pc_execute = 11'b00000000000;
  /* .\minimax.vhd:64:16  */
  always @*
    agux = n757_o; // (isignal)
  initial
    agux = 11'b00000000000;
  /* .\minimax.vhd:64:22  */
  always @*
    agua = n702_o; // (isignal)
  initial
    agua = 11'b00000000000;
  /* .\minimax.vhd:64:28  */
  always @*
    agub = n751_o; // (isignal)
  initial
    agub = 11'b00000000000;
  /* .\minimax.vhd:67:16  */
  always @*
    bubble = n294_o; // (isignal)
  initial
    bubble = 1'b1;
  /* .\minimax.vhd:67:24  */
  always @*
    bubble1 = n797_q; // (isignal)
  initial
    bubble1 = 1'b1;
  /* .\minimax.vhd:67:33  */
  always @*
    bubble2 = n798_q; // (isignal)
  initial
    bubble2 = 1'b1;
  /* .\minimax.vhd:68:16  */
  always @*
    branch_taken = n305_o; // (isignal)
  initial
    branch_taken = 1'b0;
  /* .\minimax.vhd:68:30  */
  always @*
    microcode = n799_q; // (isignal)
  initial
    microcode = 1'b0;
  /* .\minimax.vhd:69:16  */
  always @*
    trap = n286_o; // (isignal)
  initial
    trap = 1'b0;
  /* .\minimax.vhd:69:22  */
  always @*
    op16_trap = n285_o; // (isignal)
  initial
    op16_trap = 1'b0;
  /* .\minimax.vhd:69:33  */
  always @*
    op32_trap = op32; // (isignal)
  initial
    op32_trap = 1'b0;
  /* .\minimax.vhd:72:16  */
  always @*
    wb = n776_o; // (isignal)
  initial
    wb = 1'b0;
  /* .\minimax.vhd:73:16  */
  always @*
    dra = n800_q; // (isignal)
  initial
    dra = 5'b00000;
  /* .\minimax.vhd:76:16  */
  always @*
    op16 = n255_o; // (isignal)
  initial
    op16 = 1'b0;
  /* .\minimax.vhd:77:16  */
  always @*
    op16_addi4spn = n68_o; // (isignal)
  initial
    op16_addi4spn = 1'b0;
  /* .\minimax.vhd:78:16  */
  always @*
    op16_lw = n74_o; // (isignal)
  initial
    op16_lw = 1'b0;
  /* .\minimax.vhd:78:25  */
  always @*
    dly16_lw = n801_q; // (isignal)
  initial
    dly16_lw = 1'b0;
  /* .\minimax.vhd:79:16  */
  always @*
    op16_sw = n80_o; // (isignal)
  initial
    op16_sw = 1'b0;
  /* .\minimax.vhd:81:16  */
  always @*
    op16_addi = n86_o; // (isignal)
  initial
    op16_addi = 1'b0;
  /* .\minimax.vhd:82:16  */
  always @*
    op16_jal = n92_o; // (isignal)
  initial
    op16_jal = 1'b0;
  /* .\minimax.vhd:83:16  */
  always @*
    op16_li = n98_o; // (isignal)
  initial
    op16_li = 1'b0;
  /* .\minimax.vhd:84:16  */
  always @*
    op16_addi16sp = n104_o; // (isignal)
  initial
    op16_addi16sp = 1'b0;
  /* .\minimax.vhd:85:16  */
  always @*
    op16_lui = n112_o; // (isignal)
  initial
    op16_lui = 1'b0;
  /* .\minimax.vhd:87:16  */
  always @*
    op16_srli = n118_o; // (isignal)
  initial
    op16_srli = 1'b0;
  /* .\minimax.vhd:88:16  */
  always @*
    op16_srai = n124_o; // (isignal)
  initial
    op16_srai = 1'b0;
  /* .\minimax.vhd:89:16  */
  always @*
    op16_andi = n130_o; // (isignal)
  initial
    op16_andi = 1'b0;
  /* .\minimax.vhd:90:16  */
  always @*
    op16_sub = n136_o; // (isignal)
  initial
    op16_sub = 1'b0;
  /* .\minimax.vhd:91:16  */
  always @*
    op16_xor = n142_o; // (isignal)
  initial
    op16_xor = 1'b0;
  /* .\minimax.vhd:92:16  */
  always @*
    op16_or = n148_o; // (isignal)
  initial
    op16_or = 1'b0;
  /* .\minimax.vhd:93:16  */
  always @*
    op16_and = n154_o; // (isignal)
  initial
    op16_and = 1'b0;
  /* .\minimax.vhd:94:16  */
  always @*
    op16_j = n160_o; // (isignal)
  initial
    op16_j = 1'b0;
  /* .\minimax.vhd:95:16  */
  always @*
    op16_beqz = n166_o; // (isignal)
  initial
    op16_beqz = 1'b0;
  /* .\minimax.vhd:96:16  */
  always @*
    op16_bnez = n172_o; // (isignal)
  initial
    op16_bnez = 1'b0;
  /* .\minimax.vhd:98:16  */
  always @*
    op16_slli = n178_o; // (isignal)
  initial
    op16_slli = 1'b0;
  /* .\minimax.vhd:99:16  */
  always @*
    op16_lwsp = n184_o; // (isignal)
  initial
    op16_lwsp = 1'b0;
  /* .\minimax.vhd:99:27  */
  always @*
    dly16_lwsp = n802_q; // (isignal)
  initial
    dly16_lwsp = 1'b0;
  /* .\minimax.vhd:100:16  */
  always @*
    op16_jr = n190_o; // (isignal)
  initial
    op16_jr = 1'b0;
  /* .\minimax.vhd:101:16  */
  always @*
    op16_mv = n198_o; // (isignal)
  initial
    op16_mv = 1'b0;
  /* .\minimax.vhd:103:16  */
  always @*
    op16_ebreak = n204_o; // (isignal)
  initial
    op16_ebreak = 1'b0;
  /* .\minimax.vhd:104:16  */
  always @*
    op16_jalr = n212_o; // (isignal)
  initial
    op16_jalr = 1'b0;
  /* .\minimax.vhd:105:16  */
  always @*
    op16_add = n222_o; // (isignal)
  initial
    op16_add = 1'b0;
  /* .\minimax.vhd:106:16  */
  always @*
    op16_swsp = n228_o; // (isignal)
  initial
    op16_swsp = 1'b0;
  /* .\minimax.vhd:108:16  */
  always @*
    op16_slli_setrd = n234_o; // (isignal)
  initial
    op16_slli_setrd = 1'b0;
  /* .\minimax.vhd:108:33  */
  always @*
    dly16_slli_setrd = n803_q; // (isignal)
  initial
    dly16_slli_setrd = 1'b0;
  /* .\minimax.vhd:109:16  */
  always @*
    op16_slli_setrs = n240_o; // (isignal)
  initial
    op16_slli_setrs = 1'b0;
  /* .\minimax.vhd:109:33  */
  always @*
    dly16_slli_setrs = n804_q; // (isignal)
  initial
    dly16_slli_setrs = 1'b0;
  /* .\minimax.vhd:110:16  */
  always @*
    op16_slli_thunk = n246_o; // (isignal)
  initial
    op16_slli_thunk = 1'b0;
  /* .\minimax.vhd:113:16  */
  always @*
    op32 = n250_o; // (isignal)
  initial
    op32 = 1'b0;
  /* .\minimax.vhd:117:33  */
  assign n65_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:117:33  */
  assign n66_o = n65_o == 16'b0000000000000000;
  /* .\minimax.vhd:117:64  */
  assign n67_o = ~bubble;
  /* .\minimax.vhd:117:60  */
  assign n68_o = n66_o & n67_o;
  /* .\minimax.vhd:118:41  */
  assign n71_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:118:41  */
  assign n72_o = n71_o == 16'b0100000000000000;
  /* .\minimax.vhd:118:72  */
  assign n73_o = ~bubble;
  /* .\minimax.vhd:118:68  */
  assign n74_o = n72_o & n73_o;
  /* .\minimax.vhd:119:41  */
  assign n77_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:119:41  */
  assign n78_o = n77_o == 16'b1100000000000000;
  /* .\minimax.vhd:119:72  */
  assign n79_o = ~bubble;
  /* .\minimax.vhd:119:68  */
  assign n80_o = n78_o & n79_o;
  /* .\minimax.vhd:121:33  */
  assign n83_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:121:33  */
  assign n84_o = n83_o == 16'b0000000000000001;
  /* .\minimax.vhd:121:64  */
  assign n85_o = ~bubble;
  /* .\minimax.vhd:121:60  */
  assign n86_o = n84_o & n85_o;
  /* .\minimax.vhd:122:33  */
  assign n89_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:122:33  */
  assign n90_o = n89_o == 16'b0010000000000001;
  /* .\minimax.vhd:122:64  */
  assign n91_o = ~bubble;
  /* .\minimax.vhd:122:60  */
  assign n92_o = n90_o & n91_o;
  /* .\minimax.vhd:123:41  */
  assign n95_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:123:41  */
  assign n96_o = n95_o == 16'b0100000000000001;
  /* .\minimax.vhd:123:72  */
  assign n97_o = ~bubble;
  /* .\minimax.vhd:123:68  */
  assign n98_o = n96_o & n97_o;
  /* .\minimax.vhd:124:33  */
  assign n101_o = inst & 16'b1110111110000011;
  /* .\minimax.vhd:124:33  */
  assign n102_o = n101_o == 16'b0110000100000001;
  /* .\minimax.vhd:124:64  */
  assign n103_o = ~bubble;
  /* .\minimax.vhd:124:60  */
  assign n104_o = n102_o & n103_o;
  /* .\minimax.vhd:125:33  */
  assign n107_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:125:33  */
  assign n108_o = n107_o == 16'b0110000000000001;
  /* .\minimax.vhd:125:64  */
  assign n109_o = ~bubble;
  /* .\minimax.vhd:125:60  */
  assign n110_o = n108_o & n109_o;
  /* .\minimax.vhd:125:79  */
  assign n111_o = ~op16_addi16sp;
  /* .\minimax.vhd:125:75  */
  assign n112_o = n110_o & n111_o;
  /* .\minimax.vhd:127:33  */
  assign n115_o = inst & 16'b1111110001111111;
  /* .\minimax.vhd:127:33  */
  assign n116_o = n115_o == 16'b1000000000000101;
  /* .\minimax.vhd:127:64  */
  assign n117_o = ~bubble;
  /* .\minimax.vhd:127:60  */
  assign n118_o = n116_o & n117_o;
  /* .\minimax.vhd:128:33  */
  assign n121_o = inst & 16'b1111110001111111;
  /* .\minimax.vhd:128:33  */
  assign n122_o = n121_o == 16'b1000010000000101;
  /* .\minimax.vhd:128:64  */
  assign n123_o = ~bubble;
  /* .\minimax.vhd:128:60  */
  assign n124_o = n122_o & n123_o;
  /* .\minimax.vhd:129:33  */
  assign n127_o = inst & 16'b1110110000000011;
  /* .\minimax.vhd:129:33  */
  assign n128_o = n127_o == 16'b1000100000000001;
  /* .\minimax.vhd:129:64  */
  assign n129_o = ~bubble;
  /* .\minimax.vhd:129:60  */
  assign n130_o = n128_o & n129_o;
  /* .\minimax.vhd:130:33  */
  assign n133_o = inst & 16'b1110110001100011;
  /* .\minimax.vhd:130:33  */
  assign n134_o = n133_o == 16'b1000110000000001;
  /* .\minimax.vhd:130:64  */
  assign n135_o = ~bubble;
  /* .\minimax.vhd:130:60  */
  assign n136_o = n134_o & n135_o;
  /* .\minimax.vhd:131:33  */
  assign n139_o = inst & 16'b1110110001100011;
  /* .\minimax.vhd:131:33  */
  assign n140_o = n139_o == 16'b1000110000100001;
  /* .\minimax.vhd:131:64  */
  assign n141_o = ~bubble;
  /* .\minimax.vhd:131:60  */
  assign n142_o = n140_o & n141_o;
  /* .\minimax.vhd:132:41  */
  assign n145_o = inst & 16'b1110110001100011;
  /* .\minimax.vhd:132:41  */
  assign n146_o = n145_o == 16'b1000110001000001;
  /* .\minimax.vhd:132:72  */
  assign n147_o = ~bubble;
  /* .\minimax.vhd:132:68  */
  assign n148_o = n146_o & n147_o;
  /* .\minimax.vhd:133:33  */
  assign n151_o = inst & 16'b1110110001100011;
  /* .\minimax.vhd:133:33  */
  assign n152_o = n151_o == 16'b1000110001100001;
  /* .\minimax.vhd:133:64  */
  assign n153_o = ~bubble;
  /* .\minimax.vhd:133:60  */
  assign n154_o = n152_o & n153_o;
  /* .\minimax.vhd:134:33  */
  assign n157_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:134:33  */
  assign n158_o = n157_o == 16'b1010000000000001;
  /* .\minimax.vhd:134:64  */
  assign n159_o = ~bubble;
  /* .\minimax.vhd:134:60  */
  assign n160_o = n158_o & n159_o;
  /* .\minimax.vhd:135:33  */
  assign n163_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:135:33  */
  assign n164_o = n163_o == 16'b1100000000000001;
  /* .\minimax.vhd:135:64  */
  assign n165_o = ~bubble;
  /* .\minimax.vhd:135:60  */
  assign n166_o = n164_o & n165_o;
  /* .\minimax.vhd:136:33  */
  assign n169_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:136:33  */
  assign n170_o = n169_o == 16'b1110000000000001;
  /* .\minimax.vhd:136:64  */
  assign n171_o = ~bubble;
  /* .\minimax.vhd:136:60  */
  assign n172_o = n170_o & n171_o;
  /* .\minimax.vhd:138:33  */
  assign n175_o = inst & 16'b1111000001111111;
  /* .\minimax.vhd:138:33  */
  assign n176_o = n175_o == 16'b0000000000000110;
  /* .\minimax.vhd:138:64  */
  assign n177_o = ~bubble;
  /* .\minimax.vhd:138:60  */
  assign n178_o = n176_o & n177_o;
  /* .\minimax.vhd:139:33  */
  assign n181_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:139:33  */
  assign n182_o = n181_o == 16'b0100000000000010;
  /* .\minimax.vhd:139:64  */
  assign n183_o = ~bubble;
  /* .\minimax.vhd:139:60  */
  assign n184_o = n182_o & n183_o;
  /* .\minimax.vhd:140:41  */
  assign n187_o = inst & 16'b1111000001111111;
  /* .\minimax.vhd:140:41  */
  assign n188_o = n187_o == 16'b1000000000000010;
  /* .\minimax.vhd:140:72  */
  assign n189_o = ~bubble;
  /* .\minimax.vhd:140:68  */
  assign n190_o = n188_o & n189_o;
  /* .\minimax.vhd:141:41  */
  assign n193_o = inst & 16'b1111000000000011;
  /* .\minimax.vhd:141:41  */
  assign n194_o = n193_o == 16'b1000000000000010;
  /* .\minimax.vhd:141:72  */
  assign n195_o = ~bubble;
  /* .\minimax.vhd:141:68  */
  assign n196_o = n194_o & n195_o;
  /* .\minimax.vhd:141:87  */
  assign n197_o = ~op16_jr;
  /* .\minimax.vhd:141:83  */
  assign n198_o = n196_o & n197_o;
  /* .\minimax.vhd:142:33  */
  assign n201_o = inst & 16'b1111111111111111;
  /* .\minimax.vhd:142:33  */
  assign n202_o = n201_o == 16'b1001000000000010;
  /* .\minimax.vhd:142:64  */
  assign n203_o = ~bubble;
  /* .\minimax.vhd:142:60  */
  assign n204_o = n202_o & n203_o;
  /* .\minimax.vhd:143:33  */
  assign n207_o = inst & 16'b1111000001111111;
  /* .\minimax.vhd:143:33  */
  assign n208_o = n207_o == 16'b1001000000000010;
  /* .\minimax.vhd:143:64  */
  assign n209_o = ~bubble;
  /* .\minimax.vhd:143:60  */
  assign n210_o = n208_o & n209_o;
  /* .\minimax.vhd:143:79  */
  assign n211_o = ~op16_ebreak;
  /* .\minimax.vhd:143:75  */
  assign n212_o = n210_o & n211_o;
  /* .\minimax.vhd:144:33  */
  assign n215_o = inst & 16'b1111000000000011;
  /* .\minimax.vhd:144:33  */
  assign n216_o = n215_o == 16'b1001000000000010;
  /* .\minimax.vhd:144:64  */
  assign n217_o = ~bubble;
  /* .\minimax.vhd:144:60  */
  assign n218_o = n216_o & n217_o;
  /* .\minimax.vhd:144:79  */
  assign n219_o = ~op16_jalr;
  /* .\minimax.vhd:144:75  */
  assign n220_o = n218_o & n219_o;
  /* .\minimax.vhd:144:97  */
  assign n221_o = ~op16_ebreak;
  /* .\minimax.vhd:144:93  */
  assign n222_o = n220_o & n221_o;
  /* .\minimax.vhd:145:33  */
  assign n225_o = inst & 16'b1110000000000011;
  /* .\minimax.vhd:145:33  */
  assign n226_o = n225_o == 16'b1100000000000010;
  /* .\minimax.vhd:145:64  */
  assign n227_o = ~bubble;
  /* .\minimax.vhd:145:60  */
  assign n228_o = n226_o & n227_o;
  /* .\minimax.vhd:148:41  */
  assign n231_o = inst & 16'b1111000001111111;
  /* .\minimax.vhd:148:41  */
  assign n232_o = n231_o == 16'b0001000000000110;
  /* .\minimax.vhd:148:72  */
  assign n233_o = ~bubble;
  /* .\minimax.vhd:148:68  */
  assign n234_o = n232_o & n233_o;
  /* .\minimax.vhd:149:41  */
  assign n237_o = inst & 16'b1111000001111111;
  /* .\minimax.vhd:149:41  */
  assign n238_o = n237_o == 16'b0001000000001010;
  /* .\minimax.vhd:149:72  */
  assign n239_o = ~bubble;
  /* .\minimax.vhd:149:68  */
  assign n240_o = n238_o & n239_o;
  /* .\minimax.vhd:150:41  */
  assign n243_o = inst & 16'b1111000001111111;
  /* .\minimax.vhd:150:41  */
  assign n244_o = n243_o == 16'b0001000000010010;
  /* .\minimax.vhd:150:72  */
  assign n245_o = ~bubble;
  /* .\minimax.vhd:150:68  */
  assign n246_o = n244_o & n245_o;
  /* .\minimax.vhd:153:25  */
  assign n247_o = inst[1:0];
  /* .\minimax.vhd:153:17  */
  assign n248_o = &(n247_o);
  /* .\minimax.vhd:153:42  */
  assign n249_o = ~bubble;
  /* .\minimax.vhd:153:38  */
  assign n250_o = n248_o & n249_o;
  /* .\minimax.vhd:154:26  */
  assign n251_o = inst[1:0];
  /* .\minimax.vhd:154:17  */
  assign n252_o = &(n251_o);
  /* .\minimax.vhd:154:17  */
  assign n253_o = ~n252_o;
  /* .\minimax.vhd:154:43  */
  assign n254_o = ~bubble;
  /* .\minimax.vhd:154:39  */
  assign n255_o = n253_o & n254_o;
  /* .\minimax.vhd:160:31  */
  assign n256_o = op16_addi4spn | op16_lw;
  /* .\minimax.vhd:160:42  */
  assign n257_o = n256_o | op16_sw;
  /* .\minimax.vhd:160:53  */
  assign n258_o = n257_o | op16_addi;
  /* .\minimax.vhd:161:27  */
  assign n259_o = n258_o | op16_jal;
  /* .\minimax.vhd:161:39  */
  assign n260_o = n259_o | op16_li;
  /* .\minimax.vhd:161:50  */
  assign n261_o = n260_o | op16_addi16sp;
  /* .\minimax.vhd:161:67  */
  assign n262_o = n261_o | op16_lui;
  /* .\minimax.vhd:161:79  */
  assign n263_o = n262_o | op16_srli;
  /* .\minimax.vhd:162:27  */
  assign n264_o = n263_o | op16_srai;
  /* .\minimax.vhd:162:40  */
  assign n265_o = n264_o | op16_andi;
  /* .\minimax.vhd:162:53  */
  assign n266_o = n265_o | op16_sub;
  /* .\minimax.vhd:162:65  */
  assign n267_o = n266_o | op16_xor;
  /* .\minimax.vhd:162:77  */
  assign n268_o = n267_o | op16_or;
  /* .\minimax.vhd:162:88  */
  assign n269_o = n268_o | op16_and;
  /* .\minimax.vhd:162:100  */
  assign n270_o = n269_o | op16_j;
  /* .\minimax.vhd:162:110  */
  assign n271_o = n270_o | op16_beqz;
  /* .\minimax.vhd:162:123  */
  assign n272_o = n271_o | op16_bnez;
  /* .\minimax.vhd:162:136  */
  assign n273_o = n272_o | op16_slli;
  /* .\minimax.vhd:163:27  */
  assign n274_o = n273_o | op16_lwsp;
  /* .\minimax.vhd:163:40  */
  assign n275_o = n274_o | op16_jr;
  /* .\minimax.vhd:163:51  */
  assign n276_o = n275_o | op16_mv;
  /* .\minimax.vhd:163:62  */
  assign n277_o = n276_o | op16_ebreak;
  /* .\minimax.vhd:163:77  */
  assign n278_o = n277_o | op16_jalr;
  /* .\minimax.vhd:163:90  */
  assign n279_o = n278_o | op16_add;
  /* .\minimax.vhd:163:102  */
  assign n280_o = n279_o | op16_swsp;
  /* .\minimax.vhd:163:115  */
  assign n281_o = n280_o | op16_slli_setrd;
  /* .\minimax.vhd:164:33  */
  assign n282_o = n281_o | op16_slli_setrs;
  /* .\minimax.vhd:164:52  */
  assign n283_o = n282_o | op16_slli_thunk;
  /* .\minimax.vhd:159:31  */
  assign n284_o = ~n283_o;
  /* .\minimax.vhd:159:27  */
  assign n285_o = op16 & n284_o;
  /* .\minimax.vhd:166:27  */
  assign n286_o = op16_trap | op32_trap;
  /* .\minimax.vhd:175:27  */
  assign n287_o = op16_lwsp | op16_lw;
  /* .\minimax.vhd:176:38  */
  assign n288_o = op16_swsp | op16_sw;
  /* .\minimax.vhd:176:23  */
  assign n290_o = {{3{n288_o}}, n288_o}; // sext
  /* .\minimax.vhd:176:23  */
  assign n291_o = 4'b1111 & n290_o;
  /* .\minimax.vhd:179:48  */
  assign n293_o = {pc_fetch, 1'b0};
  /* .\minimax.vhd:182:27  */
  assign n294_o = bubble1 | bubble2;
  /* .\minimax.vhd:184:41  */
  assign n295_o = |(regs);
  /* .\minimax.vhd:184:41  */
  assign n296_o = ~n295_o;
  /* .\minimax.vhd:184:36  */
  assign n297_o = op16_beqz & n296_o;
  /* .\minimax.vhd:185:36  */
  assign n298_o = |(regs);
  /* .\minimax.vhd:185:31  */
  assign n299_o = op16_bnez & n298_o;
  /* .\minimax.vhd:185:17  */
  assign n300_o = n297_o | n299_o;
  /* .\minimax.vhd:186:17  */
  assign n301_o = n300_o | op16_j;
  /* .\minimax.vhd:186:27  */
  assign n302_o = n301_o | op16_jal;
  /* .\minimax.vhd:186:39  */
  assign n303_o = n302_o | op16_jr;
  /* .\minimax.vhd:186:50  */
  assign n304_o = n303_o | op16_jalr;
  /* .\minimax.vhd:187:17  */
  assign n305_o = n304_o | op16_slli_thunk;
  /* .\minimax.vhd:193:46  */
  assign n308_o = ~reset;
  /* .\minimax.vhd:193:42  */
  assign n309_o = {{10{n308_o}}, n308_o}; // sext
  /* .\minimax.vhd:193:42  */
  assign n310_o = agux & n309_o;
  /* .\minimax.vhd:196:42  */
  assign n311_o = reset | branch_taken;
  /* .\minimax.vhd:196:58  */
  assign n312_o = n311_o | trap;
  /* .\minimax.vhd:199:42  */
  assign n313_o = reset | bubble2;
  /* .\minimax.vhd:199:53  */
  assign n314_o = n313_o | n287_o;
  /* .\minimax.vhd:201:25  */
  assign n317_o = n287_o ? 1'b0 : 1'b1;
  /* .\minimax.vhd:201:25  */
  assign n318_o = n287_o ? pc_fetch_dly : pc_fetch;
  /* .\minimax.vhd:201:25  */
  assign n319_o = n287_o ? pc_execute : pc_fetch_dly;
  /* .\minimax.vhd:209:49  */
  assign n320_o = microcode | trap;
  /* .\minimax.vhd:209:73  */
  assign n321_o = reset | op16_slli_thunk;
  /* .\minimax.vhd:209:62  */
  assign n322_o = ~n321_o;
  /* .\minimax.vhd:209:58  */
  assign n323_o = n320_o & n322_o;
  /* .\minimax.vhd:211:47  */
  assign n324_o = microcode & trap;
  /* .\minimax.vhd:211:32  */
  assign n325_o = ~n324_o;
  /* .\minimax.vhd:211:25  */
  assign n327_o = ~1'b1;
  /* .\minimax.vhd:211:25  */
  assign n328_o = n327_o | n325_o;
  // /* .\minimax.vhd:211:25  */
  // always @*
  //   if (!n343_q)
  //     $fatal(1, "assertion failure n329");
  /* .\minimax.vhd:189:9  */
  always @(posedge clk)
    n343_q <= n328_o;
  initial
    n343_q = 1'b1;
  /* .\minimax.vhd:227:37  */
  assign n346_o = regd[4:0];
  /* .\minimax.vhd:227:71  */
  assign n347_o = op16_slli_setrd | op16_slli_setrs;
  /* .\minimax.vhd:227:50  */
  assign n348_o = {{4{n347_o}}, n347_o}; // sext
  /* .\minimax.vhd:227:50  */
  assign n349_o = n346_o & n348_o;
  /* .\minimax.vhd:228:49  */
  assign n350_o = inst[4:2];
  /* .\minimax.vhd:228:43  */
  assign n352_o = {2'b01, n350_o};
  /* .\minimax.vhd:228:63  */
  assign n353_o = {{4{op16_lw}}, op16_lw}; // sext
  /* .\minimax.vhd:228:63  */
  assign n354_o = n352_o & n353_o;
  /* .\minimax.vhd:228:33  */
  assign n355_o = n349_o | n354_o;
  /* .\minimax.vhd:229:42  */
  assign n356_o = inst[11:7];
  /* .\minimax.vhd:229:72  */
  assign n357_o = op16_lwsp | op32;
  /* .\minimax.vhd:229:57  */
  assign n358_o = {{4{n357_o}}, n357_o}; // sext
  /* .\minimax.vhd:229:57  */
  assign n359_o = n356_o & n358_o;
  /* .\minimax.vhd:229:33  */
  assign n360_o = n355_o | n359_o;
  /* .\minimax.vhd:234:57  */
  assign n367_o = dly16_slli_setrd | dly16_lw;
  /* .\minimax.vhd:234:69  */
  assign n368_o = n367_o | dly16_lwsp;
  /* .\minimax.vhd:234:35  */
  assign n369_o = {{4{n368_o}}, n368_o}; // sext
  /* .\minimax.vhd:234:35  */
  assign n370_o = dra & n369_o;
  /* .\minimax.vhd:235:51  */
  assign n371_o = op16_jal | op16_jalr;
  /* .\minimax.vhd:235:64  */
  assign n372_o = n371_o | trap;
  /* .\minimax.vhd:235:37  */
  assign n374_o = {{4{n372_o}}, n372_o}; // sext
  /* .\minimax.vhd:235:37  */
  assign n375_o = 5'b00001 & n374_o;
  /* .\minimax.vhd:235:25  */
  assign n376_o = n370_o | n375_o;
  /* .\minimax.vhd:236:41  */
  assign n377_o = inst[4:2];
  /* .\minimax.vhd:236:35  */
  assign n379_o = {2'b01, n377_o};
  /* .\minimax.vhd:236:74  */
  assign n380_o = op16_addi4spn | op16_sw;
  /* .\minimax.vhd:236:55  */
  assign n381_o = {{4{n380_o}}, n380_o}; // sext
  /* .\minimax.vhd:236:55  */
  assign n382_o = n379_o & n381_o;
  /* .\minimax.vhd:236:25  */
  assign n383_o = n376_o | n382_o;
  /* .\minimax.vhd:237:33  */
  assign n384_o = inst[6:2];
  /* .\minimax.vhd:237:46  */
  assign n385_o = {{4{op16_swsp}}, op16_swsp}; // sext
  /* .\minimax.vhd:237:46  */
  assign n386_o = n384_o & n385_o;
  /* .\minimax.vhd:237:25  */
  assign n387_o = n383_o | n386_o;
  /* .\minimax.vhd:238:33  */
  assign n388_o = inst[11:7];
  /* .\minimax.vhd:238:62  */
  assign n389_o = op16_addi | op16_add;
  /* .\minimax.vhd:239:49  */
  assign n390_o = ~dly16_slli_setrd;
  /* .\minimax.vhd:239:45  */
  assign n391_o = op16_mv & n390_o;
  /* .\minimax.vhd:239:33  */
  assign n392_o = n389_o | n391_o;
  /* .\minimax.vhd:240:33  */
  assign n393_o = n392_o | op16_addi16sp;
  /* .\minimax.vhd:241:33  */
  assign n394_o = n393_o | op16_slli_setrd;
  /* .\minimax.vhd:241:52  */
  assign n395_o = n394_o | op16_slli_setrs;
  /* .\minimax.vhd:242:33  */
  assign n396_o = n395_o | op16_li;
  /* .\minimax.vhd:242:44  */
  assign n397_o = n396_o | op16_lui;
  /* .\minimax.vhd:243:33  */
  assign n398_o = n397_o | op16_slli;
  /* .\minimax.vhd:238:47  */
  assign n399_o = {{4{n398_o}}, n398_o}; // sext
  /* .\minimax.vhd:238:47  */
  assign n400_o = n388_o & n399_o;
  /* .\minimax.vhd:238:25  */
  assign n401_o = n387_o | n400_o;
  /* .\minimax.vhd:244:41  */
  assign n402_o = inst[9:7];
  /* .\minimax.vhd:244:35  */
  assign n404_o = {2'b01, n402_o};
  /* .\minimax.vhd:245:33  */
  assign n405_o = op16_sub | op16_xor;
  /* .\minimax.vhd:245:45  */
  assign n406_o = n405_o | op16_or;
  /* .\minimax.vhd:245:56  */
  assign n407_o = n406_o | op16_and;
  /* .\minimax.vhd:245:68  */
  assign n408_o = n407_o | op16_andi;
  /* .\minimax.vhd:246:33  */
  assign n409_o = n408_o | op16_srli;
  /* .\minimax.vhd:246:46  */
  assign n410_o = n409_o | op16_srai;
  /* .\minimax.vhd:244:55  */
  assign n411_o = {{4{n410_o}}, n410_o}; // sext
  /* .\minimax.vhd:244:55  */
  assign n412_o = n404_o & n411_o;
  /* .\minimax.vhd:244:25  */
  assign n413_o = n401_o | n412_o;
  /* .\minimax.vhd:249:35  */
  assign n414_o = {{4{dly16_slli_setrs}}, dly16_slli_setrs}; // sext
  /* .\minimax.vhd:249:35  */
  assign n415_o = dra & n414_o;
  /* .\minimax.vhd:250:56  */
  assign n416_o = op16_addi4spn | op16_lwsp;
  /* .\minimax.vhd:250:69  */
  assign n417_o = n416_o | op16_swsp;
  /* .\minimax.vhd:250:37  */
  assign n419_o = {{4{n417_o}}, n417_o}; // sext
  /* .\minimax.vhd:250:37  */
  assign n420_o = 5'b00010 & n419_o;
  /* .\minimax.vhd:250:25  */
  assign n421_o = n415_o | n420_o;
  /* .\minimax.vhd:251:33  */
  assign n422_o = inst[11:7];
  /* .\minimax.vhd:251:60  */
  assign n423_o = op16_jr | op16_jalr;
  /* .\minimax.vhd:251:73  */
  assign n424_o = n423_o | op16_slli_thunk;
  /* .\minimax.vhd:251:92  */
  assign n425_o = n424_o | op16_slli;
  /* .\minimax.vhd:251:47  */
  assign n426_o = {{4{n425_o}}, n425_o}; // sext
  /* .\minimax.vhd:251:47  */
  assign n427_o = n422_o & n426_o;
  /* .\minimax.vhd:251:25  */
  assign n428_o = n421_o | n427_o;
  /* .\minimax.vhd:252:41  */
  assign n429_o = inst[9:7];
  /* .\minimax.vhd:252:35  */
  assign n431_o = {2'b01, n429_o};
  /* .\minimax.vhd:252:68  */
  assign n432_o = op16_sw | op16_lw;
  /* .\minimax.vhd:252:79  */
  assign n433_o = n432_o | op16_beqz;
  /* .\minimax.vhd:252:92  */
  assign n434_o = n433_o | op16_bnez;
  /* .\minimax.vhd:252:55  */
  assign n435_o = {{4{n434_o}}, n434_o}; // sext
  /* .\minimax.vhd:252:55  */
  assign n436_o = n431_o & n435_o;
  /* .\minimax.vhd:252:25  */
  assign n437_o = n428_o | n436_o;
  /* .\minimax.vhd:253:41  */
  assign n438_o = inst[4:2];
  /* .\minimax.vhd:253:35  */
  assign n440_o = {2'b01, n438_o};
  /* .\minimax.vhd:253:69  */
  assign n441_o = op16_and | op16_or;
  /* .\minimax.vhd:253:80  */
  assign n442_o = n441_o | op16_xor;
  /* .\minimax.vhd:253:92  */
  assign n443_o = n442_o | op16_sub;
  /* .\minimax.vhd:253:55  */
  assign n444_o = {{4{n443_o}}, n443_o}; // sext
  /* .\minimax.vhd:253:55  */
  assign n445_o = n440_o & n444_o;
  /* .\minimax.vhd:253:25  */
  assign n446_o = n437_o | n445_o;
  /* .\minimax.vhd:254:33  */
  assign n447_o = inst[6:2];
  /* .\minimax.vhd:254:64  */
  assign n448_o = ~dly16_slli_setrs;
  /* .\minimax.vhd:254:60  */
  assign n449_o = op16_mv & n448_o;
  /* .\minimax.vhd:254:86  */
  assign n450_o = n449_o | op16_add;
  /* .\minimax.vhd:254:46  */
  assign n451_o = {{4{n450_o}}, n450_o}; // sext
  /* .\minimax.vhd:254:46  */
  assign n452_o = n447_o & n451_o;
  /* .\minimax.vhd:254:25  */
  assign addrS_4_0 = n446_o | n452_o;
  /* .\minimax.vhd:257:32  */
  assign n454_o = microcode ^ dly16_slli_setrd;
  /* .\minimax.vhd:257:54  */
  assign n455_o = n454_o | trap;
  /* .\minimax.vhd:258:32  */
  assign microcode_xor_dly16_slli_setrs = microcode ^ dly16_slli_setrs;
  /* .\minimax.vhd:258:54  */
  assign addrS_5 = microcode_xor_dly16_slli_setrs | trap;
  /* .\minimax.vhd:264:37  */
  assign n468_o = op16_add | op16_addi;
  /* .\minimax.vhd:264:50  */
  assign n469_o = n468_o | op16_sub;
  /* .\minimax.vhd:265:25  */
  assign n470_o = n469_o | op16_and;
  /* .\minimax.vhd:265:37  */
  assign n471_o = n470_o | op16_andi;
  /* .\minimax.vhd:266:25  */
  assign n472_o = n471_o | op16_or;
  /* .\minimax.vhd:266:36  */
  assign n473_o = n472_o | op16_xor;
  /* .\minimax.vhd:267:25  */
  assign n474_o = n473_o | op16_addi16sp;
  /* .\minimax.vhd:268:25  */
  assign n475_o = n474_o | op16_slli;
  /* .\minimax.vhd:268:38  */
  assign n476_o = n475_o | op16_srli;
  /* .\minimax.vhd:268:51  */
  assign n477_o = n476_o | op16_srai;
  /* .\minimax.vhd:264:23  */
  assign n478_o = {{31{n477_o}}, n477_o}; // sext
  /* .\minimax.vhd:264:23  */
  assign n479_o = regd & n478_o;
  /* .\minimax.vhd:269:35  */
  assign n480_o = inst[10:7];
  /* .\minimax.vhd:269:29  */
  assign n482_o = {22'b0000000000000000000000, n480_o};
  /* .\minimax.vhd:269:55  */
  assign n483_o = inst[12:11];
  /* .\minimax.vhd:269:49  */
  assign n484_o = {n482_o, n483_o};
  /* .\minimax.vhd:269:76  */
  assign n485_o = inst[5];
  /* .\minimax.vhd:269:70  */
  assign n486_o = {n484_o, n485_o};
  /* .\minimax.vhd:269:86  */
  assign n487_o = inst[6];
  /* .\minimax.vhd:269:80  */
  assign n488_o = {n486_o, n487_o};
  /* .\minimax.vhd:269:90  */
  assign n490_o = {n488_o, 2'b00};
  /* .\minimax.vhd:269:98  */
  assign n491_o = {{31{op16_addi4spn}}, op16_addi4spn}; // sext
  /* .\minimax.vhd:269:98  */
  assign n492_o = n490_o & n491_o;
  /* .\minimax.vhd:269:17  */
  assign n493_o = n479_o | n492_o;
  /* .\minimax.vhd:270:66  */
  assign n494_o = inst[8:7];
  /* .\minimax.vhd:270:60  */
  assign n496_o = {24'b000000000000000000000000, n494_o};
  /* .\minimax.vhd:270:85  */
  assign n497_o = inst[12:9];
  /* .\minimax.vhd:270:79  */
  assign n498_o = {n496_o, n497_o};
  /* .\minimax.vhd:270:99  */
  assign n500_o = {n498_o, 2'b00};
  /* .\minimax.vhd:270:107  */
  assign n501_o = {{31{op16_swsp}}, op16_swsp}; // sext
  /* .\minimax.vhd:270:107  */
  assign n502_o = n500_o & n501_o;
  /* .\minimax.vhd:270:17  */
  assign n503_o = n493_o | n502_o;
  /* .\minimax.vhd:271:66  */
  assign n504_o = inst[3:2];
  /* .\minimax.vhd:271:60  */
  assign n506_o = {24'b000000000000000000000000, n504_o};
  /* .\minimax.vhd:271:85  */
  assign n507_o = inst[12];
  /* .\minimax.vhd:271:79  */
  assign n508_o = {n506_o, n507_o};
  /* .\minimax.vhd:271:96  */
  assign n509_o = inst[6:4];
  /* .\minimax.vhd:271:90  */
  assign n510_o = {n508_o, n509_o};
  /* .\minimax.vhd:271:109  */
  assign n512_o = {n510_o, 2'b00};
  /* .\minimax.vhd:271:117  */
  assign n513_o = {{31{op16_lwsp}}, op16_lwsp}; // sext
  /* .\minimax.vhd:271:117  */
  assign n514_o = n512_o & n513_o;
  /* .\minimax.vhd:271:17  */
  assign n515_o = n503_o | n514_o;
  /* .\minimax.vhd:272:35  */
  assign n516_o = inst[5];
  /* .\minimax.vhd:272:29  */
  assign n518_o = {25'b0000000000000000000000000, n516_o};
  /* .\minimax.vhd:272:45  */
  assign n519_o = inst[12:10];
  /* .\minimax.vhd:272:39  */
  assign n520_o = {n518_o, n519_o};
  /* .\minimax.vhd:272:66  */
  assign n521_o = inst[6];
  /* .\minimax.vhd:272:60  */
  assign n522_o = {n520_o, n521_o};
  /* .\minimax.vhd:272:70  */
  assign n524_o = {n522_o, 2'b00};
  /* .\minimax.vhd:272:91  */
  assign n525_o = op16_lw | op16_sw;
  /* .\minimax.vhd:272:78  */
  assign n526_o = {{31{n525_o}}, n525_o}; // sext
  /* .\minimax.vhd:272:78  */
  assign n527_o = n524_o & n526_o;
  /* .\minimax.vhd:272:17  */
  assign n528_o = n515_o | n527_o;
  /* .\minimax.vhd:275:59  */
  assign n529_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n530_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n531_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n532_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n533_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n534_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n535_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n536_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n537_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n538_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n539_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n540_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n541_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n542_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n543_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n544_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n545_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n546_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n547_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n548_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n549_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n550_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n551_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n552_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n553_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n554_o = inst[12];
  /* .\minimax.vhd:275:59  */
  assign n555_o = inst[12];
  assign n556_o = {n555_o, n554_o, n553_o, n552_o};
  assign n557_o = {n551_o, n550_o, n549_o, n548_o};
  assign n558_o = {n547_o, n546_o, n545_o, n544_o};
  assign n559_o = {n543_o, n542_o, n541_o, n540_o};
  assign n560_o = {n539_o, n538_o, n537_o, n536_o};
  assign n561_o = {n535_o, n534_o, n533_o, n532_o};
  assign n562_o = {n531_o, n530_o, n529_o};
  assign n563_o = {n556_o, n557_o, n558_o, n559_o};
  assign n564_o = {n560_o, n561_o, n562_o};
  assign n565_o = {n563_o, n564_o};
  /* .\minimax.vhd:275:71  */
  assign n566_o = inst[6:2];
  /* .\minimax.vhd:275:65  */
  assign n567_o = {n565_o, n566_o};
  /* .\minimax.vhd:275:100  */
  assign n568_o = op16_addi | op16_andi;
  /* .\minimax.vhd:275:113  */
  assign n569_o = n568_o | op16_li;
  /* .\minimax.vhd:275:85  */
  assign n570_o = {{31{n569_o}}, n569_o}; // sext
  /* .\minimax.vhd:275:85  */
  assign n571_o = n567_o & n570_o;
  /* .\minimax.vhd:275:17  */
  assign n572_o = regs | n571_o;
  /* .\minimax.vhd:276:60  */
  assign n573_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n574_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n575_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n576_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n577_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n578_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n579_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n580_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n581_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n582_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n583_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n584_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n585_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n586_o = inst[12];
  /* .\minimax.vhd:276:60  */
  assign n587_o = inst[12];
  assign n588_o = {n587_o, n586_o, n585_o, n584_o};
  assign n589_o = {n583_o, n582_o, n581_o, n580_o};
  assign n590_o = {n579_o, n578_o, n577_o, n576_o};
  assign n591_o = {n575_o, n574_o, n573_o};
  assign n592_o = {n588_o, n589_o, n590_o, n591_o};
  /* .\minimax.vhd:276:72  */
  assign n593_o = inst[6:2];
  /* .\minimax.vhd:276:66  */
  assign n594_o = {n592_o, n593_o};
  /* .\minimax.vhd:276:85  */
  assign n596_o = {n594_o, 12'b000000000000};
  /* .\minimax.vhd:276:95  */
  assign n597_o = {{31{op16_lui}}, op16_lui}; // sext
  /* .\minimax.vhd:276:95  */
  assign n598_o = n596_o & n597_o;
  /* .\minimax.vhd:276:17  */
  assign n599_o = n572_o | n598_o;
  /* .\minimax.vhd:277:59  */
  assign n600_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n601_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n602_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n603_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n604_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n605_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n606_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n607_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n608_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n609_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n610_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n611_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n612_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n613_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n614_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n615_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n616_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n617_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n618_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n619_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n620_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n621_o = inst[12];
  /* .\minimax.vhd:277:59  */
  assign n622_o = inst[12];
  assign n623_o = {n622_o, n621_o, n620_o, n619_o};
  assign n624_o = {n618_o, n617_o, n616_o, n615_o};
  assign n625_o = {n614_o, n613_o, n612_o, n611_o};
  assign n626_o = {n610_o, n609_o, n608_o, n607_o};
  assign n627_o = {n606_o, n605_o, n604_o, n603_o};
  assign n628_o = {n602_o, n601_o, n600_o};
  assign n629_o = {n623_o, n624_o, n625_o, n626_o};
  assign n630_o = {n627_o, n628_o};
  assign n631_o = {n629_o, n630_o};
  /* .\minimax.vhd:277:71  */
  assign n632_o = inst[4:3];
  /* .\minimax.vhd:277:65  */
  assign n633_o = {n631_o, n632_o};
  /* .\minimax.vhd:277:90  */
  assign n634_o = inst[5];
  /* .\minimax.vhd:277:84  */
  assign n635_o = {n633_o, n634_o};
  /* .\minimax.vhd:277:100  */
  assign n636_o = inst[2];
  /* .\minimax.vhd:277:94  */
  assign n637_o = {n635_o, n636_o};
  /* .\minimax.vhd:277:110  */
  assign n638_o = inst[6];
  /* .\minimax.vhd:277:104  */
  assign n639_o = {n637_o, n638_o};
  /* .\minimax.vhd:277:114  */
  assign n641_o = {n639_o, 4'b0000};
  /* .\minimax.vhd:277:122  */
  assign n642_o = {{31{op16_addi16sp}}, op16_addi16sp}; // sext
  /* .\minimax.vhd:277:122  */
  assign n643_o = n641_o & n642_o;
  /* .\minimax.vhd:277:17  */
  assign n644_o = n599_o | n643_o;
  /* .\minimax.vhd:280:47  */
  assign n645_o = alua - alub;
  /* .\minimax.vhd:280:63  */
  assign n646_o = op16_sub ? n645_o : n647_o;
  /* .\minimax.vhd:281:52  */
  assign n647_o = alua + alub;
  /* .\minimax.vhd:284:34  */
  assign n648_o = op16_add | op16_sub;
  /* .\minimax.vhd:284:46  */
  assign n649_o = n648_o | op16_addi;
  /* .\minimax.vhd:285:25  */
  assign n650_o = n649_o | op16_li;
  /* .\minimax.vhd:285:36  */
  assign n651_o = n650_o | op16_lui;
  /* .\minimax.vhd:286:25  */
  assign n652_o = n651_o | op16_addi4spn;
  /* .\minimax.vhd:286:42  */
  assign n653_o = n652_o | op16_addi16sp;
  /* .\minimax.vhd:287:25  */
  assign n654_o = n653_o | op16_slli;
  /* .\minimax.vhd:283:23  */
  assign n655_o = {{31{n654_o}}, n654_o}; // sext
  /* .\minimax.vhd:283:23  */
  assign n656_o = alus & n655_o;
  /* .\minimax.vhd:288:38  */
  assign n657_o = alua[31];
  /* .\minimax.vhd:288:30  */
  assign n658_o = op16_srai & n657_o;
  /* .\minimax.vhd:288:50  */
  assign n659_o = alua[31:1];
  /* .\minimax.vhd:288:44  */
  assign n660_o = {n658_o, n659_o};
  /* .\minimax.vhd:288:80  */
  assign n661_o = op16_srai | op16_srli;
  /* .\minimax.vhd:288:65  */
  assign n662_o = {{31{n661_o}}, n661_o}; // sext
  /* .\minimax.vhd:288:65  */
  assign n663_o = n660_o & n662_o;
  /* .\minimax.vhd:287:40  */
  assign n664_o = n656_o | n663_o;
  /* .\minimax.vhd:289:24  */
  assign n665_o = alua & alub;
  /* .\minimax.vhd:289:49  */
  assign n666_o = op16_andi | op16_and;
  /* .\minimax.vhd:289:34  */
  assign n667_o = {{31{n666_o}}, n666_o}; // sext
  /* .\minimax.vhd:289:34  */
  assign n668_o = n665_o & n667_o;
  /* .\minimax.vhd:288:95  */
  assign n669_o = n664_o | n668_o;
  /* .\minimax.vhd:290:24  */
  assign n670_o = alua ^ alub;
  /* .\minimax.vhd:290:34  */
  assign n671_o = {{31{op16_xor}}, op16_xor}; // sext
  /* .\minimax.vhd:290:34  */
  assign n672_o = n670_o & n671_o;
  /* .\minimax.vhd:289:63  */
  assign n673_o = n669_o | n672_o;
  /* .\minimax.vhd:291:24  */
  assign n674_o = alua | alub;
  /* .\minimax.vhd:291:46  */
  assign n675_o = op16_or | op16_mv;
  /* .\minimax.vhd:291:33  */
  assign n676_o = {{31{n675_o}}, n675_o}; // sext
  /* .\minimax.vhd:291:33  */
  assign n677_o = n674_o & n676_o;
  /* .\minimax.vhd:290:50  */
  assign n678_o = n673_o | n677_o;
  /* .\minimax.vhd:292:38  */
  assign n679_o = dly16_lw | dly16_lwsp;
  /* .\minimax.vhd:292:24  */
  assign n680_o = {{31{n679_o}}, n679_o}; // sext
  /* .\minimax.vhd:292:24  */
  assign n681_o = rdata & n680_o;
  /* .\minimax.vhd:291:59  */
  assign n682_o = n678_o | n681_o;
  /* .\minimax.vhd:293:55  */
  assign n684_o = {pc_fetch_dly, 1'b0};
  /* .\minimax.vhd:293:35  */
  assign n685_o = {20'b0, n684_o};  //  uext
  /* .\minimax.vhd:293:80  */
  assign n686_o = op16_jal | op16_jalr;
  /* .\minimax.vhd:293:93  */
  assign n687_o = n686_o | trap;
  /* .\minimax.vhd:293:66  */
  assign n688_o = {{31{n687_o}}, n687_o}; // sext
  /* .\minimax.vhd:293:66  */
  assign n689_o = n685_o & n688_o;
  /* .\minimax.vhd:292:54  */
  assign n690_o = n682_o | n689_o;
  /* .\minimax.vhd:296:41  */
  assign n691_o = trap | branch_taken;
  /* .\minimax.vhd:296:31  */
  assign n692_o = ~n691_o;
  /* .\minimax.vhd:296:27  */
  assign n693_o = {{10{n692_o}}, n692_o}; // sext
  /* .\minimax.vhd:296:27  */
  assign n694_o = pc_fetch & n693_o;
  /* .\minimax.vhd:297:32  */
  assign n695_o = {{10{branch_taken}}, branch_taken}; // sext
  /* .\minimax.vhd:297:32  */
  assign n696_o = pc_execute & n695_o;
  /* .\minimax.vhd:297:66  */
  assign n697_o = op16_jr | op16_jalr;
  /* .\minimax.vhd:297:79  */
  assign n698_o = n697_o | op16_slli_thunk;
  /* .\minimax.vhd:297:53  */
  assign n699_o = ~n698_o;
  /* .\minimax.vhd:297:49  */
  assign n700_o = {{10{n699_o}}, n699_o}; // sext
  /* .\minimax.vhd:297:49  */
  assign n701_o = n696_o & n700_o;
  /* .\minimax.vhd:297:17  */
  assign n702_o = n694_o | n701_o;
  /* .\minimax.vhd:299:31  */
  assign n703_o = regs[11:1];
  /* .\minimax.vhd:299:58  */
  assign n704_o = op16_jr | op16_jalr;
  /* .\minimax.vhd:299:71  */
  assign n705_o = n704_o | op16_slli_thunk;
  /* .\minimax.vhd:299:45  */
  assign n706_o = {{10{n705_o}}, n705_o}; // sext
  /* .\minimax.vhd:299:45  */
  assign n707_o = n703_o & n706_o;
  /* .\minimax.vhd:300:75  */
  assign n708_o = inst[12];
  /* .\minimax.vhd:300:87  */
  assign n709_o = inst[8];
  /* .\minimax.vhd:300:81  */
  assign n710_o = {n708_o, n709_o};
  /* .\minimax.vhd:300:97  */
  assign n711_o = inst[10:9];
  /* .\minimax.vhd:300:91  */
  assign n712_o = {n710_o, n711_o};
  /* .\minimax.vhd:300:117  */
  assign n713_o = inst[6];
  /* .\minimax.vhd:300:111  */
  assign n714_o = {n712_o, n713_o};
  /* .\minimax.vhd:300:127  */
  assign n715_o = inst[7];
  /* .\minimax.vhd:300:121  */
  assign n716_o = {n714_o, n715_o};
  /* .\minimax.vhd:300:137  */
  assign n717_o = inst[2];
  /* .\minimax.vhd:300:131  */
  assign n718_o = {n716_o, n717_o};
  /* .\minimax.vhd:300:147  */
  assign n719_o = inst[11];
  /* .\minimax.vhd:300:141  */
  assign n720_o = {n718_o, n719_o};
  /* .\minimax.vhd:300:158  */
  assign n721_o = inst[5:3];
  /* .\minimax.vhd:300:152  */
  assign n722_o = {n720_o, n721_o};
  /* .\minimax.vhd:301:25  */
  assign n723_o = {{10{branch_taken}}, branch_taken}; // sext
  /* .\minimax.vhd:301:25  */
  assign n724_o = n722_o & n723_o;
  /* .\minimax.vhd:301:54  */
  assign n725_o = op16_j | op16_jal;
  /* .\minimax.vhd:301:42  */
  assign n726_o = {{10{n725_o}}, n725_o}; // sext
  /* .\minimax.vhd:301:42  */
  assign n727_o = n724_o & n726_o;
  /* .\minimax.vhd:300:17  */
  assign n728_o = n707_o | n727_o;
  /* .\minimax.vhd:302:74  */
  assign n729_o = inst[12];
  /* .\minimax.vhd:302:74  */
  assign n730_o = inst[12];
  /* .\minimax.vhd:302:74  */
  assign n731_o = inst[12];
  /* .\minimax.vhd:302:74  */
  assign n732_o = inst[12];
  assign n733_o = {n729_o, n730_o, n731_o, n732_o};
  /* .\minimax.vhd:302:86  */
  assign n734_o = inst[6:5];
  /* .\minimax.vhd:302:80  */
  assign n735_o = {n733_o, n734_o};
  /* .\minimax.vhd:302:105  */
  assign n736_o = inst[2];
  /* .\minimax.vhd:302:99  */
  assign n737_o = {n735_o, n736_o};
  /* .\minimax.vhd:302:115  */
  assign n738_o = inst[11:10];
  /* .\minimax.vhd:302:109  */
  assign n739_o = {n737_o, n738_o};
  /* .\minimax.vhd:302:136  */
  assign n740_o = inst[4:3];
  /* .\minimax.vhd:302:130  */
  assign n741_o = {n739_o, n740_o};
  /* .\minimax.vhd:303:25  */
  assign n742_o = {{10{branch_taken}}, branch_taken}; // sext
  /* .\minimax.vhd:303:25  */
  assign n743_o = n741_o & n742_o;
  /* .\minimax.vhd:303:57  */
  assign n744_o = op16_bnez | op16_beqz;
  /* .\minimax.vhd:303:42  */
  assign n745_o = {{10{n744_o}}, n744_o}; // sext
  /* .\minimax.vhd:303:42  */
  assign n746_o = n743_o & n745_o;
  /* .\minimax.vhd:302:17  */
  assign n747_o = n728_o | n746_o;
  /* .\minimax.vhd:304:55  */
  assign n749_o = {{10{trap}}, trap}; // sext
  /* .\minimax.vhd:304:55  */
  assign n750_o = 11'b00000000000 & n749_o;
  /* .\minimax.vhd:304:17  */
  assign n751_o = n747_o | n750_o;
  /* .\minimax.vhd:306:23  */
  assign n752_o = agua + agub;
  /* .\minimax.vhd:306:51  */
  assign n753_o = branch_taken | n287_o;
  /* .\minimax.vhd:306:59  */
  assign n754_o = n753_o | trap;
  /* .\minimax.vhd:306:33  */
  assign n755_o = ~n754_o;
  /* .\minimax.vhd:306:31  */
  assign n756_o = {10'b0, n755_o};  //  uext
  /* .\minimax.vhd:306:31  */
  assign n757_o = n752_o + n756_o;
  /* .\minimax.vhd:308:20  */
  assign n758_o = trap | dly16_lw;
  /* .\minimax.vhd:309:26  */
  assign n759_o = n758_o | dly16_lwsp;
  /* .\minimax.vhd:309:40  */
  assign n760_o = n759_o | op16_jal;
  /* .\minimax.vhd:310:26  */
  assign n761_o = n760_o | op16_jalr;
  /* .\minimax.vhd:310:39  */
  assign n762_o = n761_o | op16_li;
  /* .\minimax.vhd:311:25  */
  assign n763_o = n762_o | op16_lui;
  /* .\minimax.vhd:311:37  */
  assign n764_o = n763_o | op16_addi;
  /* .\minimax.vhd:312:27  */
  assign n765_o = n764_o | op16_addi4spn;
  /* .\minimax.vhd:312:44  */
  assign n766_o = n765_o | op16_addi16sp;
  /* .\minimax.vhd:312:61  */
  assign n767_o = n766_o | op16_andi;
  /* .\minimax.vhd:313:27  */
  assign n768_o = n767_o | op16_mv;
  /* .\minimax.vhd:313:38  */
  assign n769_o = n768_o | op16_add;
  /* .\minimax.vhd:313:50  */
  assign n770_o = n769_o | op16_and;
  /* .\minimax.vhd:314:26  */
  assign n771_o = n770_o | op16_or;
  /* .\minimax.vhd:314:37  */
  assign n772_o = n771_o | op16_xor;
  /* .\minimax.vhd:314:49  */
  assign n773_o = n772_o | op16_sub;
  /* .\minimax.vhd:314:61  */
  assign n774_o = n773_o | op16_slli;
  /* .\minimax.vhd:315:27  */
  assign n775_o = n774_o | op16_srli;
  /* .\minimax.vhd:315:40  */
  assign n776_o = n775_o | op16_srai;
  /* .\minimax.vhd:321:37  */
  assign n779_o = addrd[4:0];
  /* .\minimax.vhd:321:29  */
  assign n780_o = |(n779_o);
  /* .\minimax.vhd:321:51  */
  assign n781_o = n780_o & wb;
  /* .\minimax.vhd:319:17  */
  assign addrS_wire = {addrS_5, addrS_4_0};
  assign n793_o = {n455_o, n413_o};
  /* .\minimax.vhd:191:17  */
  always @(posedge clk)
    n794_q <= n310_o;
  initial
    n794_q = 11'b00000000000;
  /* .\minimax.vhd:191:17  */
  always @(posedge clk)
    n795_q <= n318_o;
  initial
    n795_q = 11'b00000000000;
  /* .\minimax.vhd:191:17  */
  always @(posedge clk)
    n796_q <= n319_o;
  initial
    n796_q = 11'b00000000000;
  /* .\minimax.vhd:191:17  */
  always @(posedge clk)
    n797_q <= n314_o;
  initial
    n797_q = 1'b1;
  /* .\minimax.vhd:191:17  */
  always @(posedge clk)
    n798_q <= n312_o;
  initial
    n798_q = 1'b1;
  /* .\minimax.vhd:191:17  */
  always @(posedge clk)
    n799_q <= n323_o;
  initial
    n799_q = 1'b0;
  /* .\minimax.vhd:219:17  */
  always @(posedge clk)
    n800_q <= n360_o;
  initial
    n800_q = 5'b00000;
  /* .\minimax.vhd:219:17  */
  always @(posedge clk)
    n801_q <= op16_lw;
  initial
    n801_q = 1'b0;
  /* .\minimax.vhd:219:17  */
  always @(posedge clk)
    n802_q <= op16_lwsp;
  initial
    n802_q = 1'b0;
  /* .\minimax.vhd:219:17  */
  always @(posedge clk)
    n803_q <= op16_slli_setrd;
  initial
    n803_q = 1'b0;
  /* .\minimax.vhd:219:17  */
  always @(posedge clk)
    n804_q <= op16_slli_setrs;
  initial
    n804_q = 1'b0;
  /* .\minimax.vhd:191:17  */
  always @(posedge clk)
    n805_q <= n317_o;
  initial
    n805_q = 1'b0;
  /* .\minimax.vhd:261:31  */
  reg [31:0] register_file[63:0] ; // memory
  initial begin
    register_file[63] = 32'b00000000000000000000000000000000;
    register_file[62] = 32'b00000000000000000000000000000000;
    register_file[61] = 32'b00000000000000000000000000000000;
    register_file[60] = 32'b00000000000000000000000000000000;
    register_file[59] = 32'b00000000000000000000000000000000;
    register_file[58] = 32'b00000000000000000000000000000000;
    register_file[57] = 32'b00000000000000000000000000000000;
    register_file[56] = 32'b00000000000000000000000000000000;
    register_file[55] = 32'b00000000000000000000000000000000;
    register_file[54] = 32'b00000000000000000000000000000000;
    register_file[53] = 32'b00000000000000000000000000000000;
    register_file[52] = 32'b00000000000000000000000000000000;
    register_file[51] = 32'b00000000000000000000000000000000;
    register_file[50] = 32'b00000000000000000000000000000000;
    register_file[49] = 32'b00000000000000000000000000000000;
    register_file[48] = 32'b00000000000000000000000000000000;
    register_file[47] = 32'b00000000000000000000000000000000;
    register_file[46] = 32'b00000000000000000000000000000000;
    register_file[45] = 32'b00000000000000000000000000000000;
    register_file[44] = 32'b00000000000000000000000000000000;
    register_file[43] = 32'b00000000000000000000000000000000;
    register_file[42] = 32'b00000000000000000000000000000000;
    register_file[41] = 32'b00000000000000000000000000000000;
    register_file[40] = 32'b00000000000000000000000000000000;
    register_file[39] = 32'b00000000000000000000000000000000;
    register_file[38] = 32'b00000000000000000000000000000000;
    register_file[37] = 32'b00000000000000000000000000000000;
    register_file[36] = 32'b00000000000000000000000000000000;
    register_file[35] = 32'b00000000000000000000000000000000;
    register_file[34] = 32'b00000000000000000000000000000000;
    register_file[33] = 32'b00000000000000000000000000000000;
    register_file[32] = 32'b00000000000000000000000000000000;
    register_file[31] = 32'b00000000000000000000000000000000;
    register_file[30] = 32'b00000000000000000000000000000000;
    register_file[29] = 32'b00000000000000000000000000000000;
    register_file[28] = 32'b00000000000000000000000000000000;
    register_file[27] = 32'b00000000000000000000000000000000;
    register_file[26] = 32'b00000000000000000000000000000000;
    register_file[25] = 32'b00000000000000000000000000000000;
    register_file[24] = 32'b00000000000000000000000000000000;
    register_file[23] = 32'b00000000000000000000000000000000;
    register_file[22] = 32'b00000000000000000000000000000000;
    register_file[21] = 32'b00000000000000000000000000000000;
    register_file[20] = 32'b00000000000000000000000000000000;
    register_file[19] = 32'b00000000000000000000000000000000;
    register_file[18] = 32'b00000000000000000000000000000000;
    register_file[17] = 32'b00000000000000000000000000000000;
    register_file[16] = 32'b00000000000000000000000000000000;
    register_file[15] = 32'b00000000000000000000000000000000;
    register_file[14] = 32'b00000000000000000000000000000000;
    register_file[13] = 32'b00000000000000000000000000000000;
    register_file[12] = 32'b00000000000000000000000000000000;
    register_file[11] = 32'b00000000000000000000000000000000;
    register_file[10] = 32'b00000000000000000000000000000000;
    register_file[9] = 32'b00000000000000000000000000000000;
    register_file[8] = 32'b00000000000000000000000000000000;
    register_file[7] = 32'b00000000000000000000000000000000;
    register_file[6] = 32'b00000000000000000000000000000000;
    register_file[5] = 32'b00000000000000000000000000000000;
    register_file[4] = 32'b00000000000000000000000000000000;
    register_file[3] = 32'b00000000000000000000000000000000;
    register_file[2] = 32'b00000000000000000000000000000000;
    register_file[1] = 32'b00000000000000000000000000000000;
    register_file[0] = 32'b00000000000000000000000000000000;
    end
  assign n807_data = register_file[addrs];
  assign n808_data = register_file[addrd];
  always @(posedge clk)
    if (n781_o)
      register_file[addrd] <= alux;
  /* .\minimax.vhd:262:31  */
  /* .\minimax.vhd:261:31  */
  /* .\minimax.vhd:322:47  */
endmodule
