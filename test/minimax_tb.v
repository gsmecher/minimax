//
// Minimax: microcoded RISC-V
//
// (c) 2022 Three-Speed Logic, Inc., all rights reserved.
//
// This testbench contains:
//
// * A minimax core,
// * A dual-port RAM connected to both instruction and data buses, and
// * Enough "peripheral" to halt the simulation on completion.
//

`timescale 1 ns / 1 ps

module minimax_tb;
    parameter MAXTICKS = 100000;
    parameter ROM_SIZE = 32'h1000;
    parameter PC_BITS = $clog2(ROM_SIZE);
    parameter MICROCODE_BASE = 32'h800;
    parameter ROM_FILENAME = "blink.mem";
    parameter OUTPUT_FILENAME = "/dev/stdout";
    parameter TRACE = 0;

    reg clk;
    reg reset;

    reg [31:0] ticks;
    reg [15:0] rom_array [0:ROM_SIZE/2-1];

    // Run clock at 10 ns
    always #10 clk <= (clk === 1'b0);

    initial begin
        clk = 0;
    end

    initial begin
`ifdef VCD_FILENAME
        $dumpfile(`VCD_FILENAME);
        $dumpvars(0, minimax_tb);
`endif

        for (integer i = 0; i < ROM_SIZE/2; i = i + 1) rom_array[i] = 16'b0;

        $readmemh(ROM_FILENAME, rom_array);
    end

    reg [15:0] inst;

    wire [PC_BITS-1:0] inst_addr;
    wire inst_ce;
    wire [31:0] addr, wdata;
    reg [31:0] rdata;
    wire [3:0] wmask;
    wire rreq;
    reg rack;

    always @(posedge clk) begin

        rdata <= {rom_array[{addr[PC_BITS-1:2], 1'b1}], rom_array[{addr[PC_BITS-1:2], 1'b0}]};
        rack <= rreq;

        if(inst_ce)
          inst <= rom_array[inst_addr[PC_BITS-1:1]];

        if (wmask[3])
            rom_array[addr[PC_BITS-1:1]+1][15:8] <= wdata[31:24];
        if (wmask[2])
            rom_array[addr[PC_BITS-1:1]+1][7:0] <= wdata[23:16];
        if (wmask[1])
            rom_array[addr[PC_BITS-1:1]][15:8] <= wdata[15:8];
        if (wmask[0])
            rom_array[addr[PC_BITS-1:1]][7:0] <= wdata[7:0];
    end

    minimax #(
        .PC_BITS(PC_BITS),
        .UC_BASE(MICROCODE_BASE)
    ) dut (
        .clk(clk),
        .reset(reset),
        .inst_addr(inst_addr),
        .inst_ce(inst_ce),
        .inst(inst),
        .addr(addr),
        .wdata(wdata),
        .rdata(rdata),
        .wmask(wmask),
        .rreq(rreq),
        .rack(rack)
    );

    initial begin
        reset = 1'b1;
        #96;
        reset = 1'b0;
    end

    integer output_fd;
    initial begin
        output_fd = $fopen(OUTPUT_FILENAME, "w");
        ticks = 0;
    end

    // Capture test outputs
    always @(posedge clk)
    begin
        // Track ticks counter and bail if we took too long
        ticks <= ticks + 1;
        if (MAXTICKS != -1 && ticks >= MAXTICKS) begin
            $warning("FAIL: Exceeded MAXTICKS of %0d", MAXTICKS);
            $finish();
        end

        if (&wmask && addr==32'hfffffff8) begin
            // Capture writes to 0xfffffff8 and dump them in hex to stdout
            $fdisplay(output_fd, "%x", wdata);
        end
        else if (&wmask && addr==32'hfffffffc) begin
            // Just exit
            $finish();
        end
    end

endmodule
