library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

library xpm;
use xpm.vcomponents.all;

-- Entity definition for the Blinker module
entity blinker is 
    port (
        clk_100mhz : in  std_logic;  -- 100 MHz clock input
        led        : out std_logic := '0'  -- LED output, initialized to '0'
    );
end blinker;

-- Behavioral architecture for the Blinker module
architecture behav of blinker is
    -- Signal definitions
    signal inst_addr : std_logic_vector(11 downto 0);
    signal inst      : std_logic_vector(15 downto 0);
    signal inst_ce   : std_logic;

    signal addr, wdata : std_logic_vector(31 downto 0);
    signal rdata       : std_logic_vector(31 downto 0) := (others => '0');
    signal wmask       : std_logic_vector(3 downto 0);
    signal rreq, rack  : std_logic := '0';

begin
    -- Memory component for instruction and data storage
    rom : xpm_memory_tdpram
    generic map (
        ADDR_WIDTH_A         => 11,
        ADDR_WIDTH_B         => 10,
        AUTO_SLEEP_TIME      => 0,
        BYTE_WRITE_WIDTH_A   => 16,
        BYTE_WRITE_WIDTH_B   => 32,
        CASCADE_HEIGHT       => 0,
        CLOCKING_MODE        => "common_clock",
        ECC_MODE             => "no_ecc",
        MEMORY_INIT_FILE     => "blink.mem",
        MEMORY_INIT_PARAM    => "0",
        MEMORY_OPTIMIZATION  => "true",
        MEMORY_PRIMITIVE     => "block",
        MEMORY_SIZE          => 32 * 1024,  -- 32K memory size
        MESSAGE_CONTROL      => 0,
        READ_DATA_WIDTH_A    => 16,
        READ_DATA_WIDTH_B    => 32,
        READ_LATENCY_A       => 1,
        READ_LATENCY_B       => 1,
        READ_RESET_VALUE_A   => "0",
        READ_RESET_VALUE_B   => "0",
        RST_MODE_A           => "SYNC",
        RST_MODE_B           => "SYNC",
        SIM_ASSERT_CHK       => 0,
        USE_EMBEDDED_CONSTRAINT => 0,
        USE_MEM_INIT         => 1,
        USE_MEM_INIT_MMI     => 1,
        WAKEUP_TIME          => "disable_sleep",
        WRITE_DATA_WIDTH_A   => 16,
        WRITE_DATA_WIDTH_B   => 32,
        WRITE_MODE_A         => "no_change",
        WRITE_MODE_B         => "no_change",
        WRITE_PROTECT        => 1
    )
    port map (
        douta => inst,
        doutb => rdata,
        addra => inst_addr(inst_addr'high downto 1),
        addrb => addr(inst_addr'high downto 2),
        clka  => clk_100mhz,
        clkb  => clk_100mhz,
        dina  => 16x"0",
        dinb  => wdata,
        ena   => inst_ce,
        enb   => rreq,
        injectdbiterra => '0',
        injectdbiterrb => '0',
        injectsbiterra => '0',
        injectsbiterrb => '0',
        regcea => '0',
        regceb => '0',
        rsta   => '0',
        rstb   => '0',
        sleep  => '0',
        wea    => "0",
        web    => "0"
    );

    -- Device under test (DUT) configuration
    dut : entity work.minimax
    generic map (
        PC_BITS => inst_addr'length,
        UC_BASE => x"00000800"  -- User configuration base address
    )
    port map (
        clk    => clk_100mhz,
        reset  => '0',
        inst_addr => inst_addr,
        inst_ce   => inst_ce,
        inst   => inst,
        addr   => addr,
        wdata  => wdata,
        rdata  => rdata,
        wmask  => wmask,
        rreq   => rreq,
        rack   => rack
    );

    -- Process to handle LED control based on memory write operations
    io_proc: process(clk_100mhz)
    begin
        if rising_edge(clk_100mhz) then
            rack <= rreq;  -- Acknowledge read requests

            -- Handle LED control when address 0xfffffffc is written
            if wmask = x"f" and addr = x"fffffffc" then
                led <= wdata(0);  -- Update LED state based on data[0]
            end if;
        end if;
    end process;

end behav;
