-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmTiming.vhd
-- Author     : Matt Weaver <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-08
-- Last update: 2019-10-28
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
      --recRxData        : out slv(15 downto 0);
      --recRxDataK       : out slv( 1 downto 0);
      --recSof           : out sl;               -- strobe one clk after SOF
      --recEof           : out sl;               -- strobe one clk after EOF
      --recCrcErr        : out sl;               -- latch one clk after CRC (on EOF)
-------------------------------------------------------------------------------
-- This file is part of 'LCLS2 XPM Core'.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'LCLS2 XPM Core', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;


library surf;
use surf.StdRtlPkg.all;
use surf.AxiStreamPkg.all;
use surf.SsiPkg.all;
use surf.AxiPkg.all;
use surf.AxiLitePkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;

library amc_carrier_core;
use amc_carrier_core.AmcCarrierPkg.all;
use amc_carrier_core.AmcCarrierSysRegPkg.all;

library l2si_core;
use l2si_core.XpmPkg.all;

library unisim;
use unisim.vcomponents.all;

entity XpmTiming is
   generic (
      TPD_G               : time            := 1 ns;
      APP_TYPE_G          : AppType         := APP_NULL_TYPE_C;
      AXI_ERROR_RESP_G    : slv(1 downto 0) := AXI_RESP_DECERR_C );
   port (
      -- AXI-Lite Interface (axilClk domain)
      axilClk          : in  sl;
      axilRst          : in  sl;
      axilReadMaster   : in  AxiLiteReadMasterType;
      axilReadSlave    : out AxiLiteReadSlaveType;
      axilWriteMaster  : in  AxiLiteWriteMasterType;
      axilWriteSlave   : out AxiLiteWriteSlaveType;
      ----------------------
      -- Top Level Interface
      ----------------------      
      -- Timing Interface 
      recTimingClk     : out sl;
      recTimingRst     : out sl;
      recTimingBus     : out TimingBusType;
      recData          : out TimingRxType;
      appTimingPhy     : in  TimingPhyType;             -- Timing feedback
      appTimingPhyClk  : out sl;
      appTimingPhyRst  : out sl;
      ----------------
      -- Core Ports --
      ----------------   
      -- LCLS Timing Ports
      timingRxEnable   : in  sl := '1';
      timingRxP        : in  sl;
      timingRxN        : in  sl;
      timingTxP        : out sl;
      timingTxN        : out sl;
      timingRefClkInP  : in  sl;
      timingRefClkInN  : in  sl;
      timingRefClkOut  : out sl;
      timingRecClkOutP : out sl;
      timingRecClkOutN : out sl;
      timingClkSel     : out sl);
end XpmTiming;

architecture mapping of XpmTiming is

   constant GTH_ADDR : slv(31 downto 0) := TIMING_ADDR_C+x"00800000";
   
   constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(1 downto 0) := (
      0               => (
         baseAddr     => TIMING_ADDR_C,
         addrBits     => 23,
         connectivity => x"FFFF"),
      1               => (
         baseAddr     => GTH_ADDR,
         addrBits     => 23,
         connectivity => x"FFFF"));  

   signal axilWriteMasters : AxiLiteWriteMasterArray(1 downto 0);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray(1 downto 0);
   signal axilReadMasters  : AxiLiteReadMasterArray(1 downto 0);
   signal axilReadSlaves   : AxiLiteReadSlaveArray(1 downto 0);

   constant NUM_COUNTERS_C  : integer := 8;
   constant COUNTER_WIDTH_C : integer := 32;

   -- Synchronized to AXIL clk
   signal axilStatusCounters : SlVectorArray(NUM_COUNTERS_C-1 downto 0, COUNTER_WIDTH_C-1 downto 0);
   signal axilRxLinkUp       : sl;
   signal stv                : slv(NUM_COUNTERS_C-1 downto 0);
   
   signal timingRefClk     : sl;

   -- Rx ports
   signal rxControl      : TimingPhyControlType;
   signal rxStatus       : TimingPhyStatusType;
   signal genStatus      : TimingPhyStatusType := (
      locked       => '1',
      resetDone    => '1',
      bufferByDone => '1',
      bufferByErr  => '0' );
   signal recStatus      : TimingPhyStatusType;

   signal rxUsrClkActive : sl;
   signal rxCdrStable    : sl;
   signal rxUsrClk       : sl;
   signal rxData         : TimingRxType;
   signal rxOutClk       : sl;
   signal txStatus       : TimingPhyStatusType := TIMING_PHY_STATUS_INIT_C;
   signal txOutClk       : sl;
   signal txUsrClk       : sl;
   signal txUsrRst       : sl;
   signal txUsrClkActive : sl;
   signal txOutClk       : sl;
   signal loopback       : slv(2 downto 0);
   signal rxRst          : sl;

   signal genTimingPhy   : TimingPhyType;
   signal genTimingRef   : sl;
   signal genTimingRefG  : sl;
begin

   --------------------------
   -- AXI-Lite: Crossbar Core
   --------------------------  
   U_XBAR : entity surf.AxiLiteCrossbar
      generic map (
         TPD_G              => TPD_G,
         DEC_ERROR_RESP_G   => AXI_ERROR_RESP_G,
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => 2,
         MASTERS_CONFIG_G   => AXI_CROSSBAR_MASTERS_CONFIG_C)
      port map (
         axiClk              => axilClk,
         axiClkRst           => axilRst,
         sAxiWriteMasters(0) => axilWriteMaster,
         sAxiWriteSlaves(0)  => axilWriteSlave,
         sAxiReadMasters(0)  => axilReadMaster,
         sAxiReadSlaves(0)   => axilReadSlave,
         mAxiWriteMasters    => axilWriteMasters,
         mAxiWriteSlaves     => axilWriteSlaves,
         mAxiReadMasters     => axilReadMasters,
         mAxiReadSlaves      => axilReadSlaves);

   timingRefClkOut  <= timingRefClk;
   recTimingClk     <= rxUsrClk;
   recTimingRst     <= rxRst;

   appTimingPhyClk  <= txUsrClk;
   appTimingPhyRst  <= txUsrRst;
   txUsrClkActive   <= '1';
   rxRst            <= not(recStatus.resetDone);

   rxUsrClkActive   <= '1';

   -------------------------------------------------------------------------------------------------
   -- Clock Buffers
   -------------------------------------------------------------------------------------------------
   TIMING_REFCLK_IBUFDS_GTE3 : IBUFDS_GTE3
      generic map (
         REFCLK_EN_TX_PATH  => '0',
         REFCLK_HROW_CK_SEL => "01",    -- 2'b01: ODIV2 = Divide-by-2 version of O
         REFCLK_ICNTL_RX    => "00")
      port map (
         I     => timingRefClkInP,
         IB    => timingRefClkInN,
         CEB   => '0',
         ODIV2 => genTimingRef,
         O     => timingRefClk);

   -------------------------------------------------------------------------------------------------
   -- GTH Timing Receiver
   -------------------------------------------------------------------------------------------------
   U_GENTIMING : BUFG_GT
     port map ( I       => genTimingRef,
                CE      => '1',
                CEMASK  => '1',
                CLR     => '0',
                CLRMASK => '1',
                DIV     => "000",           -- Divide-by-1
                O       => genTimingRefG);

   -- BUFGCTRL (asynchronous mux) 
   txUsrClk  <= genTimingRefG when timingRxEnable='0' else txOutClk;
   rxUsrClk  <= genTimingRefG when timingRxEnable='0' else rxOutClk;
   txUsrRst  <= '0'           when timingRxEnable='0' else not (txStatus.resetDone);
   recStatus <= genStatus     when timingRxEnable='0' else rxStatus;
   recData   <= genTimingPhy  when timingRxEnable='0' else rxData;
   
   TimingGthCoreWrapper_1 : entity work.TimingGthCoreWrapper
     generic map ( TPD_G            => TPD_G,
                   AXIL_BASE_ADDR_G => GTH_ADDR )
     port map (
       axilClk        => axilClk,
       axilRst        => axilRst,
       axilReadMaster => axilReadMasters(1),
       axilReadSlave  => axilReadSlaves (1),
       axilWriteMaster=> axilWriteMasters(1),
       axilWriteSlave => axilWriteSlaves (1),
       stableClk      => axilClk,
       gtRefClk       => timingRefClk,
       gtRefClkDiv2   => '0',
       gtRxP          => timingRxP,
       gtRxN          => timingRxN,
       gtTxP          => timingTxP,
       gtTxN          => timingTxN,
       rxControl      => rxControl,
       rxStatus       => rxStatus,
       rxUsrClkActive => rxUsrClkActive,
       rxCdrStable    => rxCdrStable,
       rxUsrClk       => rxUsrClk,
       rxData         => rxData.data,
       rxDataK        => rxData.dataK,
       rxDispErr      => rxData.dspErr,
       rxDecErr       => rxData.decErr,
       rxOutClk       => rxOutClk,
       txControl      => appTimingPhy.control,
       txStatus       => txStatus,
       txUsrClk       => txOutClk,
       txUsrClkActive => txUsrClkActive,
       txData         => appTimingPhy.data,
       txDataK        => appTimingPhy.dataK,
       txOutClk       => txOutClk,  -- will this be source synchronous?
       loopback       => loopback);
   
   -- Drive the external CLK MUX
   timingClkSel <= '1';

   -- Send a copy of the timing clock to the AMC's clock cleaner
   ClkOutBufDiff_Inst : entity surf.ClkOutBufDiff
      generic map (
         TPD_G        => TPD_G,
         XIL_DEVICE_G => "ULTRASCALE")
      port map (
         clkIn   => rxUsrClk,
         clkOutP => timingRecClkOutP,
         clkOutN => timingRecClkOutN);

   TimingCore_1 : entity lcls_timing_core.TimingCore
     generic map ( TPD_G             => TPD_G,
                   TPGMINI_G         => true,
                   AXIL_BASE_ADDR_G  => TIMING_ADDR_C,
                   AXIL_ERROR_RESP_G => AXI_RESP_DECERR_C )
     port map (
         gtTxUsrClk      => txUsrClk,
         gtTxUsrRst      => txUsrRst,
         gtRxRecClk      => rxOutClk,
         gtRxData        => rxData.data,
         gtRxDataK       => rxData.dataK,
         gtRxDispErr     => rxData.dspErr,
         gtRxDecErr      => rxData.decErr,
         gtRxControl     => rxControl,
         gtRxStatus      => recStatus,
         gtLoopback      => loopback,
         appTimingClk    => rxOutClk,
         appTimingRst    => rxRst,
         appTimingBus    => recTimingBus,
         timingPhy       => genTimingPhy,
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilReadMaster  => axilReadMasters(0),
         axilReadSlave   => axilReadSlaves (0),
         axilWriteMaster => axilWriteMasters(0),
         axilWriteSlave  => axilWriteSlaves (0));

end mapping;
