-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmBaseKcu1500.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2024-07-02
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Firmware Target's Top Level
--
-- This module may be configured in one of many ways depending upon the
-- application:
--
-- "XTPG"     : 119MHz LCLS-1 timing is received via the crossbar.
--              186MHz LCLS-2 timing is generated from the LCLS-1 input.
-- "XPM"      : 186MHz LCLS-2 timing is received via the RTM (FPGA_0) inputs.
-- "XTPG_UED" : 119MHz LCLS-1 timing is received via the front panel.
--              186MHz LCLS-2 timing is generated from the LCLS-1 input.
-- "XPM_UED"  : 119MHz LCLS-2 timing is received via the front panel.
--
-- The MGT clocking has the following constraints:
-- timing,AMC0 ports can only be clocked by the timingRefClk (238/371 MHz)
--      or the devClk ports.
-- rtm,AMC1 ports can only be clocked by the genRefClk (reload with 371 MHz)
--      or the devClk ports.
-- The devClk ports are driven by a Si5317A PLL with the FPGA output
-- 
-- Note: Common-to-Application interface defined here (see URL below)
--       https://confluence.slac.stanford.edu/x/rLyMCw
-------------------------------------------------------------------------------
-- This file is part of 'LCLS2 DAQ Software'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'LCLS2 DAQ Software', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

library unisim;
use unisim.vcomponents.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiStreamPkg.all;
use surf.AxiLitePkg.all;
use surf.AxiStreamPkg.all;
use surf.SsiPkg.all;
use surf.EthMacPkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;
use lcls_timing_core.TPGPkg.all;
use lcls_timing_core.TPGMiniEdefPkg.all;

library l2si_core;
use l2si_core.XpmPkg.all;
use l2si_core.XpmSeqPkg.all;

--library amc_carrier_core;
--use amc_carrier_core.AmcCarrierPkg.all;

library l2si;
use l2si.XpmAppPkg.all;

entity XpmBaseKcu1500 is
   generic (
      TPD_G               : time    := 1 ns;
      AXIL_BASE_G         : slv(31 downto 0) := (others=>'0');
      DMA_AXIS_CONFIG_G   : AxiStreamConfigType;
      XPM_MODE_G          : string := "XpmGen" );
   -- "XpmGen"   : generate timing locally
   -- "XpmAsync" : receive upstream timing and retransmit asynchronously
   port (
     -- AXI-Lite Interface (axilClk domain)
     axilClk               : out sl;
     axilRst               : out sl;
     axilReadMaster        : in  AxiLiteReadMasterType;
     axilReadSlave         : out AxiLiteReadSlaveType;
     axilWriteMaster       : in  AxiLiteWriteMasterType;
     axilWriteSlave        : out AxiLiteWriteSlaveType;
     -- AXI-Stream Interface
     dmaClk                : in  sl;
     dmaRst                : in  sl;
     obDmaMaster           : in  AxiStreamMasterType;
     obDmaSlave            : out AxiStreamSlaveType;
     ibDmaMaster           : out AxiStreamMasterType;
     ibDmaSlave            : in  AxiStreamSlaveType;
     ------------------
     --  Hardware Ports
     ------------------
     -- QSFP[0] Ports,
     qsfp0RefClkP          : in  slv(1 downto 0);
     qsfp0RefClkN          : in  slv(1 downto 0);
     qsfp0RxP              : in  slv(3 downto 0);
     qsfp0RxN              : in  slv(3 downto 0);
     qsfp0TxP              : out slv(3 downto 0);
     qsfp0TxN              : out slv(3 downto 0);
     -- QSFP[1] Ports
     qsfp1RefClkP          : in  slv(1 downto 0);
     qsfp1RefClkN          : in  slv(1 downto 0);
     qsfp1RxP              : in  slv(3 downto 0);
     qsfp1RxN              : in  slv(3 downto 0);
     qsfp1TxP              : out slv(3 downto 0);
     qsfp1TxN              : out slv(3 downto 0) );
end XpmBaseKcu1500;

architecture top_level of XpmBaseKcu1500 is

   -- AmcCarrierCore Configuration Constants
   constant DIAGNOSTIC_SIZE_C   : positive            := 1;
   constant DIAGNOSTIC_CONFIG_C : AxiStreamConfigType := ssiAxiStreamConfig(4);

   -- AXI-Lite Interface (appClk domain)
   signal regClk         : sl;
   signal regRst         : sl;
   signal uregRst        : sl;
   signal regUpdate      : slv(XPM_PARTITIONS_C-1 downto 0);

   -- Reference Clocks and Resets
   signal timingPhyClk : sl;
   signal timingPhyRst : sl := '0';
   signal timingPhyClk2 : sl;
   signal phyClk01  : sl;
   signal phyClk11  : sl;
   signal gphyClk01 : sl;
   signal gphyClk11 : sl;
   signal phyRst01  : sl;
   signal usRecClk  : sl;
   signal usRefClk  : sl;
   
   constant NUM_DS_LINKS_C : integer := 8;
   constant NUM_FP_LINKS_C : integer := 8;
   constant NUM_BP_LINKS_C : integer := 1;
 
   signal xpmConfig : XpmConfigType;
   signal xpmStatus : XpmStatusType;
   signal pattern   : XpmPatternStatisticsType;
   signal bpStatus  : XpmBpLinkStatusArray(NUM_BP_LINKS_C downto 0) := (others => XPM_BP_LINK_STATUS_INIT_C);

   signal pllStatus : XpmPllStatusArray (1 downto 0);
   signal pllLocked : slv(1 downto 0);

   signal dsClockP : slv(1 downto 0);
   signal dsClockN : slv(1 downto 0);
   signal idsRxP   : Slv7Array(1 downto 0);
   signal idsRxN   : Slv7Array(1 downto 0);
   signal idsTxP   : Slv7Array(1 downto 0);
   signal idsTxN   : Slv7Array(1 downto 0);

   signal dsLinkStatus : XpmLinkStatusArray(NUM_FP_LINKS_C-1 downto 0);
   signal dsLinkConfig : XpmLinkConfigArray(NUM_FP_LINKS_C-1 downto 0);
   signal dsLinkConfig0: XpmLinkConfigType := XPM_LINK_CONFIG_INIT_C;
   signal dsTxData     : Slv16Array(NUM_FP_LINKS_C-1 downto 0);
   signal dsTxDataK    : Slv2Array (NUM_FP_LINKS_C-1 downto 0);
   signal dsRxData     : Slv16Array(NUM_FP_LINKS_C-1 downto 0);
   signal dsRxDataK    : Slv2Array (NUM_FP_LINKS_C-1 downto 0);
   signal dsRxClk      : slv (NUM_FP_LINKS_C-1 downto 0);
   signal dsRxRst      : slv (NUM_FP_LINKS_C-1 downto 0);
   signal dsRxErr      : slv (NUM_FP_LINKS_C-1 downto 0);
   signal dsTxOutClk   : slv (NUM_FP_LINKS_C-1 downto 0);

   signal bpRxLinkUp   : slv        (NUM_BP_LINKS_C-1 downto 0) := (others=>'0');
   signal bpRxLinkFull : Slv16Array (NUM_BP_LINKS_C-1 downto 0) := (others=>x"0000");
   signal bpTxData     : Slv16Array(0 downto 0) := (others=>X"0000");
   signal bpTxDataK    : Slv2Array (0 downto 0) := (others=> "00");

   signal dbgChan   : slv(4 downto 0);
   signal dbgChanS  : slv(4 downto 0);
   signal ringData  : slv(19 downto 0);
   signal ringDataI : Slv19Array(NUM_FP_LINKS_C-1 downto 0);
   signal ringDataV : slv (NUM_FP_LINKS_C-1 downto 0);

   constant REG_INDEX_C  : integer := 0;
   constant RING_INDEX_C : integer := 1;
   constant TEST_INDEX_C : integer := 2;
   constant TIM_INDEX_C  : integer := 3;
   constant APP_INDEX_C  : integer := 4;
   constant ASYN_INDEX_C : integer := 5;
   constant AXI_XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(5 downto 0) := (
     REG_INDEX_C   => (baseAddr     => AXIL_BASE_G + X"00000000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     RING_INDEX_C  => (baseAddr     => AXIL_BASE_G + X"00010000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     TEST_INDEX_C  => (baseAddr     => AXIL_BASE_G + X"00020000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     TIM_INDEX_C   => (baseAddr     => AXIL_BASE_G + X"00030000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     APP_INDEX_C   => (baseAddr     => AXIL_BASE_G + X"00040000",
                       addrBits     => 17,
                       connectivity => X"FFFF"),
     ASYN_INDEX_C  => (baseAddr     => AXIL_BASE_G + X"00080000",
                       addrBits     => 19,
                       connectivity => X"FFFF") );

   signal axilReadMasters  : AxiLiteReadMasterArray (AXI_XBAR_CONFIG_C'range);
   signal axilReadSlaves   : AxiLiteReadSlaveArray  (AXI_XBAR_CONFIG_C'range) := (others=>AXI_LITE_READ_SLAVE_EMPTY_OK_C);
   signal axilWriteMasters : AxiLiteWriteMasterArray(AXI_XBAR_CONFIG_C'range);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray (AXI_XBAR_CONFIG_C'range) := (others=>AXI_LITE_WRITE_SLAVE_EMPTY_OK_C);

   signal ibDebugMaster : AxiStreamMasterType;
   signal ibDebugSlave  : AxiStreamSlaveType;
   signal obDebugMaster : AxiStreamMasterType;
   signal obDebugSlave  : AxiStreamSlaveType;
   signal stepMaster, seqMaster, monMaster : AxiStreamMasterType;
   signal stepSlave , seqSlave , monSlave  : AxiStreamSlaveType;

   signal dsClkBuf    : slv(1 downto 0);

   signal bpMonClk : sl := '0';
   signal ipAddr   : slv(31 downto 0);

   signal groupLinkClear         : slv(XPM_PARTITIONS_C-1 downto 0);

   -- Timing Interface (timingClk domain)
   signal recStream   : XpmStreamType;
   signal timingPhy   : TimingPhyType := TIMING_PHY_INIT_C;
   signal timingPhyId : slv(xpmConfig.paddr'range);
   signal txStreams   : TimingSerialArray(2 downto 0);
   signal txStreamIds : Slv4Array        (2 downto 0);
   signal txAdvance   : slv              (2 downto 0) := (others=>'0');
   signal txFiducial  : sl;

   constant AMC_DS_PORT0_C : IntegerArray(1 downto 0) := ( 0, 0 );
   constant AMC_DS_PORTN_C : IntegerArray(1 downto 0) := ( 3, 3 );
   
   constant AMC_DS_LINKS_C : IntegerArray(1 downto 0) := ( 4, 4 );
   constant AMC_DS_FIRST_C : IntegerArray(1 downto 0) := ( 4, 0 );
   constant AMC_DS_LAST_C  : IntegerArray(1 downto 0) :=
     ( AMC_DS_LINKS_C(1)+AMC_DS_FIRST_C(1)-1,
       AMC_DS_LINKS_C(0)+AMC_DS_FIRST_C(0)-1 );

   signal seqCountRst : sl;
   signal seqCount    : Slv128Array(XPM_SEQ_DEPTH_C-1 downto 0);
   
   signal tmpReg : slv(31 downto 0) := x"DEADBEEF";
   signal usRx   : TimingRxType;
   signal usRxControl : TimingPhyControlType;
   signal usRxStatus  : TimingPhyStatusType := TIMING_PHY_STATUS_INIT_C;

   signal common : slv(XPM_PARTITIONS_C-1 downto 0);
   
   component ila_0
     port ( clk    : in sl;
            probe0 : in slv(255 downto 0) );
   end component;
   
begin

  U_ILA : ila_0
    port map ( clk       => regClk,
               probe0(0) => dsLinkStatus(0).rxResetDone,
               probe0(1) => dsLinkStatus(0).rxReady,
               probe0(2) => dsLinkStatus(0).txResetDone,
               probe0(3) => dsLinkStatus(0).txReady,
               probe0(4) => dsRxRst(0),
               probe0(5) => dsLinkConfig(0).enable,
               probe0(6) => dsLinkConfig(0).txReset,
               probe0(7) => dsLinkConfig(0).txPllReset,
               probe0(8) => dsLinkConfig(0).rxReset,
               probe0(9) => dsLinkConfig(0).rxPllReset,
               probe0(255 downto 10) => (others=>'0') );
  
  U_ILA_RX : ila_0
    port map ( clk       => dsRxClk(0),
               probe0(15 downto  0) => dsRxData (0),
               probe0(17 downto 16) => dsRxDataK(0),
               probe0(18) => dsRxErr(0),
               probe0(255 downto 19) => (others=>'0') );
               
   axilClk <= regClk;
   axilRst <= regRst;
   --
   --  The QSFP channels are not reordered
   --
   dsClockP    <= qsfp1RefClkP(0) & qsfp0RefClkP(0);
   dsClockN    <= qsfp1RefClkN(0) & qsfp0RefClkN(0);
   qsfp0TxP    <= idsTxP(0)(3 downto 0);
   qsfp0TxN    <= idsTxN(0)(3 downto 0);
   qsfp1TxP    <= idsTxP(1)(3 downto 0);
   qsfp1TxN    <= idsTxN(1)(3 downto 0);
   idsRxP(0)(3 downto 0) <= qsfp0RxP;
   idsRxN(0)(3 downto 0) <= qsfp0RxN;
   idsRxP(1)(3 downto 0) <= qsfp1RxP;
   idsRxN(1)(3 downto 0) <= qsfp1RxN;

   U_BUFG  : BUFG_GT
     port map (  I       => dsTxOutClk(AMC_DS_FIRST_C(0)),
                 CE      => '1',
                 CEMASK  => '1',
                 CLR     => '0',
                 CLRMASK => '1',
                 DIV     => "000",
                 O       => timingPhyClk );
   
   U_BUFG2  : BUFG_GT
     port map (  I       => dsTxOutClk(AMC_DS_FIRST_C(1)),
                 CE      => '1',
                 CEMASK  => '1',
                 CLR     => '0',
                 CLRMASK => '1',
                 DIV     => "000",
                 O       => timingPhyClk2 );
   
    IBUFDS_GTE3_REFCLK01 : IBUFDS_GTE3
      generic map (
        REFCLK_EN_TX_PATH  => '0',
        REFCLK_HROW_CK_SEL => "00",    -- 2'b01: ODIV2 = Divide-by-2 version of O
        REFCLK_ICNTL_RX    => "00")
      port map (
        I     => qsfp0RefClkP(1),
        IB    => qsfp0RefClkN(1),
        CEB   => '0',
        ODIV2 => gphyClk01,
        O     => open);

   U_BUFG3  : BUFG_GT
     port map (  I       => gphyClk01,
                 CE      => '1',
                 CEMASK  => '1',
                 CLR     => '0',
                 CLRMASK => '1',
                 DIV     => "000",
                 O       => phyClk01 );

   U_PhyRst01 : entity surf.RstSync
     port map (
       clk      => phyClk01,
       asyncRst => dmaRst,
       syncRst  => phyRst01 );
   
   ---------------------------------------
   -- AXI-Lite and reference 25 MHz clocks
   ---------------------------------------
   U_axilClk : entity surf.ClockManagerUltraScale
      generic map(
         TPD_G             => TPD_G,
         SIMULATION_G      => false,
         TYPE_G            => "PLL",
         INPUT_BUFG_G      => false,
         FB_BUFG_G         => true,
         RST_IN_POLARITY_G => '1',
         NUM_CLOCKS_G      => 1,
         -- MMCM attributes
         CLKIN_PERIOD_G    => 6.4,      -- 156.25 MHz
         CLKFBOUT_MULT_G   => 8,        -- 1.25GHz = 8 x 156.25 MHz
         CLKOUT0_DIVIDE_G  => 12)       -- 104MHz = 1.25GHz/12
      port map(
         -- Clock Input
         clkIn     => phyClk01,
         rstIn     => phyRst01,
         -- Clock Outputs
         clkOut(0) => regClk,
         -- Reset Outputs
         rstOut(0) => uregRst );

    U_REGRST : BUFG
      port map (
         I  => uregRst,
         O  => regRst );
      
    IBUFDS_GTE3_REFCLK11 : IBUFDS_GTE3
      generic map (
        REFCLK_EN_TX_PATH  => '0',
        REFCLK_HROW_CK_SEL => "00",    -- 2'b01: ODIV2 = Divide-by-2 version of O
        REFCLK_ICNTL_RX    => "00")
      port map (
        I     => qsfp1RefClkP(1),
        IB    => qsfp1RefClkN(1),
        CEB   => '0',
        ODIV2 => gphyClk11,
        O     => open);

   U_TimingPhyRst : entity surf.RstSync
     generic map (
       IN_POLARITY_G => '0' )
     port map (
       clk      => timingPhyClk,
       asyncRst => tmpReg(0),
       syncRst  => timingPhyRst );
      
   U_XBAR : entity surf.AxiLiteCrossbar
      generic map (
         TPD_G              => TPD_G,
         DEC_ERROR_RESP_G   => AXI_RESP_DECERR_C,
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => AXI_XBAR_CONFIG_C'length,
         MASTERS_CONFIG_G   => AXI_XBAR_CONFIG_C)
      port map (
         axiClk              => regClk,
         axiClkRst           => regRst,
         sAxiWriteMasters(0) => axilWriteMaster,
         sAxiWriteSlaves(0)  => axilWriteSlave,
         sAxiReadMasters(0)  => axilReadMaster,
         sAxiReadSlaves (0)  => axilReadSlave,
         mAxiWriteMasters    => axilWriteMasters,
         mAxiWriteSlaves     => axilWriteSlaves,
         mAxiReadMasters     => axilReadMasters,
         mAxiReadSlaves      => axilReadSlaves);

   U_SyncDbg : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => dbgChan'length)
      port map (
         clk     => timingPhyClk,
         dataIn  => dbgChan,
         dataOut => dbgChanS);

   GEN_RINGD : for i in 0 to NUM_DS_LINKS_C-1 generate
      U_Sync : entity surf.SynchronizerFifo
         generic map (
            TPD_G        => TPD_G,
            DATA_WIDTH_G => 19)
         port map (
            wr_clk            => dsRxClk(i),
            din(18)           => dsRxErr (i),
            din(17 downto 16) => dsRxDataK(i),
            din(15 downto 0)  => dsRxData (i),
            rd_clk            => timingPhyClk,
            valid             => ringDataV(i),
            dout              => ringDataI(i));
   end generate;

   process (timingPhyClk) is
      variable iLink : integer;
   begin
      if rising_edge(timingPhyClk) then
         iLink    := conv_integer(dbgChanS);
         ringData <= ringDataV(iLink) & ringDataI(iLink) after TPD_G;
      end if;
   end process;

   AxiLiteRingBuffer_1 : entity surf.AxiLiteRingBuffer
      generic map (
         TPD_G            => TPD_G,
         MEMORY_TYPE_G    => "block",
         REG_EN_G         => true,
         DATA_WIDTH_G     => 20,
         RAM_ADDR_WIDTH_G => 13)
      port map (
         dataClk         => timingPhyClk,
         dataRst         => '0',
         dataValid       => '1',
         dataValue       => ringData,
         axilClk         => regClk,
         axilRst         => regRst,
         axilReadMaster  => axilReadMasters (RING_INDEX_C),
         axilReadSlave   => axilReadSlaves  (RING_INDEX_C),
         axilWriteMaster => axilWriteMasters(RING_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (RING_INDEX_C));

   pllLocked <= "11";

   U_Application : entity l2si.XpmApp
      generic map (
         TPD_G           => TPD_G,
         NUM_DS_LINKS_G  => NUM_DS_LINKS_C,
         NUM_BP_LINKS_G  => NUM_BP_LINKS_C,
         AXIL_BASEADDR_G => AXI_XBAR_CONFIG_C(APP_INDEX_C).baseAddr)
      port map (
         -----------------------
         -- Application Ports --
         -----------------------
         -- -- AMC's DS Ports
         dsLinkStatus    => dsLinkStatus(NUM_DS_LINKS_C-1 downto 0),
         dsRxData        => dsRxData    (NUM_DS_LINKS_C-1 downto 0),
         dsRxDataK       => dsRxDataK   (NUM_DS_LINKS_C-1 downto 0),
         dsTxData        => dsTxData    (NUM_DS_LINKS_C-1 downto 0),
         dsTxDataK       => dsTxDataK   (NUM_DS_LINKS_C-1 downto 0),
         dsRxClk         => dsRxClk     (NUM_DS_LINKS_C-1 downto 0),
         dsRxRst         => dsRxRst     (NUM_DS_LINKS_C-1 downto 0),
         dsRxErr         => dsRxErr     (NUM_DS_LINKS_C-1 downto 0),
         --  BP DS Ports
         bpTxData        => bpTxData (0),
         bpTxDataK       => bpTxDataK(0),
         bpStatus        => bpStatus,
         bpRxLinkPause   => bpRxLinkFull,
         ----------------------
         -- Top Level Interface
         ----------------------
         regclk          => regClk,
         regrst          => regRst,
         update          => regUpdate,
         status          => xpmStatus,
         pattern         => pattern,
         common          => common,
         config          => xpmConfig,
         axilReadMaster  => axilReadMasters (APP_INDEX_C),
         axilReadSlave   => axilReadSlaves  (APP_INDEX_C),
         axilWriteMaster => axilWriteMasters(APP_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (APP_INDEX_C),
         groupLinkClear  => groupLinkClear,
         -- Async Notification
         obAppMaster     => seqMaster,
         obAppSlave      => seqSlave,
         -- Timing Interface (timingClk domain) 
         timingClk       => timingPhyClk,
         timingRst       => timingPhyRst,
--         timingIn        => recTimingData,
         timingStream    => recStream,
         timingFbClk     => timingPhyClk,
         timingFbRst     => '0',
         timingFbId      => timingPhyId,
         timingFb        => timingPhy,
         seqCountRst     => seqCountRst,
         seqCount        => seqCount );

   --  Map all messages to VC=1
   U_MasterMux : entity surf.AxiStreamMux
     generic map ( NUM_SLAVES_G   => 3,
                   MODE_G         => "ROUTED",
                   TDEST_ROUTES_G => (0=>"00000001",
                                      1=>"00000001",
                                      2=>"00000001") )
     port map ( axisClk         => regClk,
                axisRst         => regRst,
                sAxisMasters(0) => seqMaster,
                sAxisMasters(1) => stepMaster,
                sAxisMasters(2) => monMaster,
                sAxisSlaves (0) => seqSlave,
                sAxisSlaves (1) => stepSlave,
                sAxisSlaves (2) => monSlave,
                mAxisMaster     => obDebugMaster,
                mAxisSlave      => obDebugSlave );

   U_AXIS_FIFO : entity surf.AxiStreamFifoV2
     generic map (
       SLAVE_AXI_CONFIG_G   => EMAC_AXIS_CONFIG_C,
       MASTER_AXI_CONFIG_G  => DMA_AXIS_CONFIG_G )
     port map (
       -- Slave Port
       sAxisClk    => regClk,
       sAxisRst    => regRst,
       sAxisMaster => obDebugMaster,
       sAxisSlave  => obDebugSlave,
       -- Master Port
       mAxisClk    => dmaClk,
       mAxisRst    => dmaRst,
       mAxisMaster => ibDmaMaster,
       mAxisSlave  => ibDmaSlave );

   U_Reg : entity l2si.XpmReg
      generic map(
         TPD_G               => TPD_G,
         NUM_DS_LINKS_G      => NUM_FP_LINKS_C,
         NUM_BP_LINKS_G      => NUM_BP_LINKS_C,
         US_RX_ENABLE_INIT_G => (XPM_MODE_G="XpmAsync"),
         CU_RX_ENABLE_INIT_G => false,
         REMOVE_MONREG_G     => true )
      port map (
         axilClk         => regClk,
         axilRst         => regRst,
         axilUpdate      => regUpdate,
         axilReadMaster  => axilReadMasters (REG_INDEX_C),
         axilReadSlave   => axilReadSlaves  (REG_INDEX_C),
         axilWriteMaster => axilWriteMasters(REG_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (REG_INDEX_C),
         groupLinkClear  => groupLinkClear,
         -- Streaming input (regClk domain)
         ibDebugMaster   => ibDebugMaster,
         ibDebugSlave    => ibDebugSlave,
         obDebugMaster   => stepMaster,
         obDebugSlave    => stepSlave,
         obMonitorMaster => monMaster,
         obMonitorSlave  => monSlave,
         staClk          => timingPhyClk,
         pllStatus       => pllStatus,
         status          => xpmStatus,
         pattern         => pattern,
         monClk(0)       => timingPhyClk2,
         monClk(1)       => timingPhyClk,
         monClk(2)       => usRecClk,
         monClk(3)       => usRefClk,
         monLatch        => seqCountRst,
         seqCount        => seqCount,
         config          => xpmConfig,
         common          => common,
         usRxEnable      => open,
         cuRxEnable      => open,
         dbgChan         => dbgChan);

   GEN_XPMGEN : if XPM_MODE_G = "XpmGen" generate
     U_XpmGen : entity l2si.XpmGenKcu1500
       port map (
         axilClk               => regClk,
         axilRst               => regRst,
         axilReadMaster        => axilReadMasters (TIM_INDEX_C),
         axilReadSlave         => axilReadSlaves  (TIM_INDEX_C),
         axilWriteMaster       => axilWriteMasters(TIM_INDEX_C),
         axilWriteSlave        => axilWriteSlaves (TIM_INDEX_C),

         timingPhyClk          => timingPhyClk,
         timingPhyRst          => timingPhyRst,
         recStream             => recStream );
     usRecClk     <= timingPhyClk;
     usRefClk     <= timingPhyClk;
     dsLinkConfig <= xpmConfig.dsLink(NUM_DS_LINKS_C-1 downto 0);
   end generate;
   

   GEN_XPMASYNC : if XPM_MODE_G = "XpmAsync" generate
     U_XpmAsync : entity l2si.XpmAsyncKcu1500
       generic map (
         TPD_G   => TPD_G,
         AXIL_BASE_G => AXI_XBAR_CONFIG_C(ASYN_INDEX_C).baseAddr )
       port map (
         axilClk               => regClk,
         axilRst               => regRst,
         axilReadMaster        => axilReadMasters (ASYN_INDEX_C),
         axilReadSlave         => axilReadSlaves  (ASYN_INDEX_C),
         axilWriteMaster       => axilWriteMasters(ASYN_INDEX_C),
         axilWriteSlave        => axilWriteSlaves (ASYN_INDEX_C),
         usRecClk              => dsRxClk(0),
         usRx                  => usRx,
         usRxStatus            => usRxStatus,
         usRxControl           => usRxControl,
         timingPhyClk          => timingPhyClk,
         timingPhyRst          => timingPhyRst,
         recStream             => recStream );
     usRxStatus.locked       <= dsLinkStatus(0).rxReady;
     usRxStatus.resetDone    <= dsLinkStatus(0).rxReady;
     usRxStatus.bufferByDone <= dsLinkStatus(0).rxResetDone;
     usRxStatus.bufferByErr  <= '0';
     usRecClk    <= dsRxClk  (0);
     usRx.data   <= dsRxData (0);
     usRx.dataK  <= dsRxDataK(0);
     usRx.dspErr <= dsRxErr(0) & dsRxErr(0);
     usRx.decErr <= "00";
     usRefClk    <= timingPhyClk;
     dsTxData (0) <= timingPhy.data;
     dsTxDataK(0) <= timingPhy.dataK;
     dsLinkConfig0.rxReset    <= usRxControl.reset;
     dsLinkConfig0.rxPllReset <= usRxControl.pllReset;
     dsLinkConfig <= xpmConfig.dsLink(NUM_DS_LINKS_C-1 downto 1) & dsLinkConfig0;
   end generate GEN_XPMASYNC;
   
   GEN_AMC_MGT : for i in 0 to 1 generate
      U_Rcvr : entity l2si.XpmGthUltrascaleWrapper
         generic map (
            GTGCLKRX   => false,
            NLINKS_G   => AMC_DS_LINKS_C(i),
            USE_IBUFDS => true)
         port map (
            stableClk => regClk,
            gtTxP     => idsTxP (i)(AMC_DS_PORTN_C(i) downto AMC_DS_PORT0_C(i)),
            gtTxN     => idsTxN (i)(AMC_DS_PORTN_C(i) downto AMC_DS_PORT0_C(i)),
            gtRxP     => idsRxP (i)(AMC_DS_PORTN_C(i) downto AMC_DS_PORT0_C(i)),
            gtRxN     => idsRxN (i)(AMC_DS_PORTN_C(i) downto AMC_DS_PORT0_C(i)),
            devClkP   => dsClockP (i),
            devClkN   => dsClockN (i),
            devClkOut => dsClkBuf (i),
            devClkBuf => dsTxOutClk(AMC_DS_FIRST_C(i)),
            txData    => dsTxData  (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            txDataK   => dsTxDataK (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            rxData    => dsRxData  (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            rxDataK   => dsRxDataK (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            rxClk     => dsRxClk   (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            rxRst     => dsRxRst   (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            rxErr     => dsRxErr   (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
--                 txOutClk        => dsTxOutClk(7*i+6 downto 7*i),
            txClk     => open,
            txClkIn   => timingPhyClk,
            txClkRst  => timingPhyRst,
            config    => dsLinkConfig(AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            status    => dsLinkStatus(AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            axilRst         => regRst,
            axilReadMaster  => AXI_LITE_READ_MASTER_INIT_C,
            axilWriteMaster => AXI_LITE_WRITE_MASTER_INIT_C );
   end generate;

   U_SyncPaddrTx : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => xpmConfig.paddr'length-4)
      port map (
         clk     => timingPhyClk,
         dataIn  => xpmConfig.paddr(xpmConfig.paddr'left-4 downto 0),
         dataOut => timingPhyId(timingPhyId'left downto 4) );
   timingPhyId(3 downto 0) <= x"F";
   
   U_AXI_EMPTY : entity surf.AxiLiteRegs
     port map (
       axiClk         => regClk,
       axiClkRst      => regRst,
       axiReadMaster  => axilReadMasters (TEST_INDEX_C),
       axiReadSlave   => axilReadSlaves  (TEST_INDEX_C),
       axiWriteMaster => axilWriteMasters(TEST_INDEX_C),
       axiWriteSlave  => axilWriteSlaves (TEST_INDEX_C),
       writeRegister(0) => tmpReg,
       readRegister (0) => tmpReg );

end top_level;

