------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmBase.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2025-06-05
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

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;

library l2si_core;
use l2si_core.XpmPkg.all;
use l2si_core.XpmSeqPkg.all;

--library amc_carrier_core;
--use amc_carrier_core.AmcCarrierPkg.all;

library l2si;
use l2si.XpmAppPkg.all;

entity XpmBase is
   generic (
      TPD_G               : time    := 1 ns;
      BUILD_INFO_G        : BuildInfoType;
      USE_RTM_G           : boolean := true;  -- Get LCLS2 input from RTM vs AMC
      USE_XTPG_G          : boolean := false; -- 
      US_RX_ENABLE_INIT_G : boolean := true;  -- Enable XPM upsteam input/feedback
      CU_RX_ENABLE_INIT_G : boolean := false; -- Enable LCLS1 input via Xbar
      CU_ASYNC_G          : boolean := false; -- Latch Cu input to nearest 1MHz
                                              -- fiducial
      UED_MODE_G          : boolean := false);
   port (
      -----------------------
      -- Application Ports --
      -----------------------
      -- -- AMC's HS Ports
      dsClkP           : in    Slv1Array(1 downto 0);
      dsClkN           : in    Slv1Array(1 downto 0);
      dsRxP            : in    Slv7Array(1 downto 0);
      dsRxN            : in    Slv7Array(1 downto 0);
      dsTxP            : out   Slv7Array(1 downto 0);
      dsTxN            : out   Slv7Array(1 downto 0);
      frqTbl           : inout slv (1 downto 0);
      frqSel           : inout Slv4Array(1 downto 0);
      bwSel            : inout Slv2Array(1 downto 0);
      inc              : out   slv (1 downto 0);
      dec              : out   slv (1 downto 0);
      sfOut            : inout Slv2Array(1 downto 0);
      rate             : inout Slv2Array(1 downto 0);
      bypass           : out   slv (1 downto 0);
      pllRst           : out   slv (1 downto 0);
      lol              : in    slv (1 downto 0);
      los              : in    slv (1 downto 0);
      hsrScl           : inout Slv3Array(1 downto 0);
      hsrSda           : inout Slv3Array(1 downto 0);
      amcScl           : inout slv (1 downto 0);
      amcSda           : inout slv (1 downto 0);
      amcRstN          : out   slv (1 downto 0);
      -- RTM LS ports
      --rtmLsP      : out   slv(35 downto 32);
      --rtmLsN      : out   slv(35 downto 32);
      ----------------
      -- Core Ports --
      ----------------   
      -- Common Fabric Clock
      fabClkP          : in    sl;
      fabClkN          : in    sl;
      -- XAUI Ports
      ethRxP           : in    slv(3 downto 0);
      ethRxN           : in    slv(3 downto 0);
      ethTxP           : out   slv(3 downto 0);
      ethTxN           : out   slv(3 downto 0);
      ethClkP          : in    sl;
      ethClkN          : in    sl;
      -- Backplane MPS Ports
      bpClkIn          : in    sl;
      bpClkOut         : out   sl;
      bpBusRxP         : in    slv(14 downto 1);
      bpBusRxN         : in    slv(14 downto 1);
      -- LCLS Timing Ports
      timingTxP        : out   sl;      -- Synchronous Timing Distribution (zone 2)
      timingTxN        : out   sl;
      timingRxP        : in    sl;      -- LCLS-I Timing Input
      timingRxN        : in    sl;
      timingRefClkInP  : in    sl;
      timingRefClkInN  : in    sl;
      -- Upstream timing reception, feedback transmission (if slave)
      usRxP            : in    sl;
      usRxN            : in    sl;
      usTxP            : out   sl;
      usTxN            : out   sl;
      usClkP           : in    sl;      -- use genRef(0) with 371MHz osc
      usClkN           : in    sl;
      --
      timingRecClkOutP : out   sl;
      timingRecClkOutN : out   sl;
      timingClkSel     : out   sl;
      timingClkScl     : inout sl;      -- jitter cleaner (unused)
      timingClkSda     : inout sl;
      fpgaclk_P        : out   slv(3 downto 0);
      fpgaclk_N        : out   slv(3 downto 0);
      --fpgaclk0_P       : out   sl;
      --fpgaclk0_N       : out   sl;
      --fpgaclk2_P       : out   sl;
      --fpgaclk2_N       : out   sl;
      -- Crossbar Ports
      xBarSin          : out   slv(1 downto 0);
      xBarSout         : out   slv(1 downto 0);
      xBarConfig       : out   sl;
      xBarLoad         : out   sl;
      -- IPMC Ports
      ipmcScl          : inout sl;
      ipmcSda          : inout sl;
      -- Configuration PROM Ports
      calScl           : inout sl;
      calSda           : inout sl;
      -- DDR3L SO-DIMM Ports
      ddrScl           : inout sl;
      ddrSda           : inout sl;
      -- SYSMON Ports
      vPIn             : in    sl;
      vNIn             : in    sl);
end XpmBase;

architecture top_level of XpmBase is

   -- AmcCarrierCore Configuration Constants
   constant DIAGNOSTIC_SIZE_C   : positive            := 1;
   constant DIAGNOSTIC_CONFIG_C : AxiStreamConfigType := ssiAxiStreamConfig(4);

   constant NUM_SEQ_C : natural := 8;
   constant NUM_DDC_C : integer := 0;

   -- AXI-Lite Interface (appClk domain)
   signal regClk         : sl;
   signal regRst         : sl;
   signal regUpdate      : slv(XPM_PARTITIONS_C-1 downto 0);
   signal regReadMaster  : AxiLiteReadMasterType;
   signal regReadSlave   : AxiLiteReadSlaveType;
   signal regWriteMaster : AxiLiteWriteMasterType;
   signal regWriteSlave  : AxiLiteWriteSlaveType;

   -- Reference Clocks and Resets
   signal timingPhyClk : sl;
   signal recTimingClk : sl;
   signal recTimingRst : sl;
   signal recTimingLkN : sl;
   signal ref125MHzClk : sl;
   signal ref125MHzRst : sl;
   signal ref156MHzClk : sl;
   signal ref156MHzRst : sl;

   constant NUM_FP_LINKS_C : integer := 14;

   signal xpmConfig  : XpmConfigType;
   signal xpmStatus  : XpmStatusType;
   signal pattern    : XpmPatternStatisticsType;
   signal patternCfg : XpmPatternConfigType;
   signal pllStatus  : XpmPllStatusArray (1 downto 0);
   signal pllLocked  : slv(1 downto 0);
   signal txClkRst   : slv(1 downto 0);
   
   signal dsClockP : slv(1 downto 0);
   signal dsClockN : slv(1 downto 0);
   signal idsRxP   : Slv7Array(1 downto 0);
   signal idsRxN   : Slv7Array(1 downto 0);
   signal idsTxP   : Slv7Array(1 downto 0);
   signal idsTxN   : Slv7Array(1 downto 0);

   signal dsLinkStatus : XpmLinkStatusArray(NUM_FP_LINKS_C-1 downto 0) := (others=>XPM_LINK_STATUS_INIT_C);
   signal dsTxData     : Slv16Array(NUM_FP_LINKS_C-1 downto 0);
   signal dsTxDataK    : Slv2Array (NUM_FP_LINKS_C-1 downto 0);
   signal dsRxData     : Slv16Array(NUM_FP_LINKS_C-1 downto 0);
   signal dsRxDataK    : Slv2Array (NUM_FP_LINKS_C-1 downto 0);
   signal dsRxClk      : slv (NUM_FP_LINKS_C-1 downto 0);
   signal dsRxRst      : slv (NUM_FP_LINKS_C-1 downto 0);
   signal dsRxErr      : slv (NUM_FP_LINKS_C-1 downto 0) := (others=>'0');
   signal dsTxOutClk   : slv (NUM_FP_LINKS_C-1 downto 0);

   signal dbgChan   : slv(4 downto 0);
   signal dbgChanS  : slv(4 downto 0);
   signal ringData  : slv(19 downto 0);
   signal ringDataI : Slv19Array(NUM_FP_LINKS_C-1 downto 0);
   signal ringDataV : slv (NUM_FP_LINKS_C-1 downto 0);

   constant REG_INDEX_C  : integer := 0;
   constant RING_INDEX_C : integer := 1;
   constant TEST_INDEX_C : integer := 2;
   constant MMCM_INDEX_C : integer := 3;
   constant APP_INDEX_C  : integer := 4;
   constant PHAS_INDEX_C : integer := 5;
   constant AMC_GTH_INDEX_C: integer := 6; 
   constant AMC_GTH1_INDEX_C: integer := 7; 
   
   constant AXI_XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(7 downto 0) := (
     REG_INDEX_C   => (baseAddr     => X"80000000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     RING_INDEX_C  => (baseAddr     => X"80010000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     TEST_INDEX_C  => (baseAddr     => X"80020000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     MMCM_INDEX_C  => (baseAddr     => X"80030000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     APP_INDEX_C   => (baseAddr     => X"80040000",
                       addrBits     => 17,
                       connectivity => X"FFFF"),
     PHAS_INDEX_C  => (baseAddr     => X"80080000",
                       addrBits     => 19,
                       connectivity => X"FFFF"),
     AMC_GTH_INDEX_C  => (baseAddr     => X"80100000",
                       addrBits     => 19,
                       connectivity => X"FFFF"),
     AMC_GTH1_INDEX_C  => (baseAddr     => X"80200000",
                       addrBits     => 19,
                       connectivity => X"FFFF") );

   signal axilReadMasters  : AxiLiteReadMasterArray (AXI_XBAR_CONFIG_C'range);
   signal axilReadSlaves   : AxiLiteReadSlaveArray (AXI_XBAR_CONFIG_C'range) := (others=>AXI_LITE_READ_SLAVE_EMPTY_OK_C);
   signal axilWriteMasters : AxiLiteWriteMasterArray(AXI_XBAR_CONFIG_C'range);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray (AXI_XBAR_CONFIG_C'range) := (others=>AXI_LITE_WRITE_SLAVE_EMPTY_OK_C);

   signal ibDebugMaster : AxiStreamMasterType;
   signal ibDebugSlave  : AxiStreamSlaveType;
   signal obDebugMaster : AxiStreamMasterType;
   signal obDebugSlave  : AxiStreamSlaveType;
   signal stepMaster, seqMaster, monMaster : AxiStreamMasterType;
   signal stepSlave , seqSlave , monSlave  : AxiStreamSlaveType;

   signal dsClkBuf    : slv(1 downto 0);
   signal fpgaclk_ret : sl;

   signal ipAddr   : slv(31 downto 0);

--   signal cuRxFiducial, cuSync : sl;
   signal cuSync        : sl;
   signal cuRecClk      : sl;
   signal cuRecFiducial : sl;

   signal usRxEnable, cuRxEnable : sl;
   signal groupLinkClear         : slv(XPM_PARTITIONS_C-1 downto 0);

   -- Timing Interface (timingClk domain)
   --signal recTimingDataI : TimingRxType;
   --signal recTimingData  : TimingRxType;
   signal recStream : XpmStreamType;
   signal timingPhy : TimingPhyType;
   signal timingPhyId : slv(xpmConfig.paddr'range);

   signal itimingTxP, itimingTxN : sl;
   signal itimingRxP, itimingRxN : sl;
   signal timingRefClk, itimingRefClk, timingRefClkGt : sl;
   signal usRefClk, usRefClkO, usRefClkGt : sl;
   
   signal iusTxP, iusTxN : sl;
   signal iusRxP, iusRxN : sl;
   signal iusRefClk, iusRefClkGt : sl;

   constant AMC_DS_PORT0_C : IntegerArray(1 downto 0) :=
     ( 0, ite(USE_RTM_G,0,1) );
   constant AMC_DS_PORTN_C : IntegerArray(1 downto 0) :=
     ( 6, 6 );
   
   constant AMC_DS_LINKS_C : IntegerArray(1 downto 0) :=
     ( 7, ite(USE_RTM_G,7,6) );
   constant AMC_DS_FIRST_C : IntegerArray(1 downto 0) :=
     ( 7, ite(USE_RTM_G,0,1) );
   constant AMC_DS_LAST_C  : IntegerArray(1 downto 0) :=
     ( AMC_DS_LINKS_C(1)+AMC_DS_FIRST_C(1)-1,
       AMC_DS_LINKS_C(0)+AMC_DS_FIRST_C(0)-1 );

   signal seqCountRst : sl;
   signal seqCount    : Slv128Array(NUM_DDC_C+NUM_SEQ_C-1 downto 0);
   signal seqRestart  : slv(NUM_SEQ_C-1 downto 0);
   signal seqDisable  : slv(NUM_SEQ_C-1 downto 0);
   
   signal tmpReg : slv(31 downto 0) := x"DEADBEEF";

   signal common : slv(XPM_PARTITIONS_C-1 downto 0);
   signal timeStamp : slv(63 downto 0);
   
begin

   amcRstN <= "11";

   --
   --  The AMC SFP channels are reordered - the mapping to MGT quads is non-trivial
   --    amcTx/Rx indexed by MGT
   --    iamcTx/Rx indexed by SFP
   --
   reorder_p : process (dsClkP, dsClkN, dsRxP, dsRxN, idsTxP, idsTxN) is
   begin
      for i in 0 to 1 loop
         dsClockP(i) <= dsClkP(i)(0);
         dsClockN(i) <= dsClkN(i)(0);
         for j in 0 to 3 loop
            dsTxP(i)(j)     <= idsTxP(i)(j+2);
            dsTxN(i)(j)     <= idsTxN(i)(j+2);
            idsRxP (i)(j+2) <= dsRxP(i)(j);
            idsRxN (i)(j+2) <= dsRxN(i)(j);
         end loop;
         for j in 4 to 5 loop
            dsTxP(i)(j)     <= idsTxP(i)(j-4);
            dsTxN(i)(j)     <= idsTxN(i)(j-4);
            idsRxP (i)(j-4) <= dsRxP(i)(j);
            idsRxN (i)(j-4) <= dsRxN(i)(j);
         end loop;
         for j in 6 to 6 loop
            dsTxP(i)(j)   <= idsTxP(i)(j);
            dsTxN(i)(j)   <= idsTxN(i)(j);
            idsRxP (i)(j) <= dsRxP(i)(j);
            idsRxN (i)(j) <= dsRxN(i)(j);
         end loop;
      end loop;
   end process;

   TIMREFCLK_IBUFDS_GTE3 : IBUFDS_GTE3
     generic map (
       REFCLK_EN_TX_PATH  => '0',
       REFCLK_HROW_CK_SEL => "01",    -- 2'b01: ODIV2 = Divide-by-2 version of O
       REFCLK_ICNTL_RX    => "00")
     port map (
       I     => timingRefClkInP,
       IB    => timingRefClkInN,
       CEB   => '0',
       ODIV2 => itimingRefClk,   -- 119 MHz
       O     => timingRefClkGt); -- 238 MHz

   U_BUFG_GT : BUFG_GT
     port map ( O       => timingRefClk,
                CE      => '1',
                CEMASK  => '1',
                CLR     => '0',
                CLRMASK => '1',
                DIV     => "000",           -- Divide-by-1
                I       => itimingRefClk );

   TIMING_REFCLK_IBUFDS_GTE3 : IBUFDS_GTE3
      generic map (
         REFCLK_EN_TX_PATH  => '0',
         REFCLK_HROW_CK_SEL => "01",    -- 2'b01: ODIV2 = Divide-by-2 version of O
         REFCLK_ICNTL_RX    => "00")
      port map (
         I     => usClkP,
         IB    => usClkN,
         CEB   => '0',
         ODIV2 => usRefClkO,
         O     => usRefClkGt);

   GEN_USREFCLK : if USE_RTM_G or not US_RX_ENABLE_INIT_G generate
     U_USREFCLK : BUFG_GT
       port map (I       => usRefClkO,
                 CE      => '1',
                 CEMASK  => '1',
                 CLR     => '0',
                 CLRMASK => '1',
                 DIV     => "000",       -- Divide-by-
                 O       => usRefClk);
   end generate;

   GEN_RTM : if USE_RTM_G generate
     itimingRxP  <= timingRxP;
     itimingRxN  <= timingRxN;
     timingTxP   <= itimingTxP;
     timingTxN   <= itimingTxN;
     iusRxP      <= usRxP;
     iusRxN      <= usRxN;
     usTxP       <= iusTxP;
     usTxN       <= iusTxN;
     iusRefClk   <= usRefClk;
     iusRefClkGt <= usRefClkGt;
   end generate;
   GEN_NO_RTM : if not USE_RTM_G generate
     --  No RTM
     --  Is dsRx(0) CuTiming or UsTiming
     GEN_US_RX: if US_RX_ENABLE_INIT_G generate
       itimingRxP   <= timingRxP;
       itimingRxN   <= timingRxN;
       timingTxP    <= itimingTxP;
       timingTxN    <= itimingTxN;
       iusRxP       <= idsRxP(0)(0);
       iusRxN       <= idsRxN(0)(0);
       idsTxP(0)(0) <= iusTxP;
       idsTxN(0)(0) <= iusTxN;
       iusRefClk    <= timingRefClk;
       iusRefClkGt  <= timingRefClkGt;
       U_GTH : entity surf.Gthe3ChannelDummy
         port map ( refClk   => dsClkBuf(1),
                    gtRxP(0) => usRxP,
                    gtRxN(0) => usRxN,
                    gtTxP(0) => usTxP,
                    gtTxN(0) => usTxN );
     end generate GEN_US_RX;
     GEN_CU_RX: if not US_RX_ENABLE_INIT_G generate
       iusRxP       <= usRxP;
       iusRxN       <= usRxN;
       usTxP        <= iusTxP;
       usTxN        <= iusTxN;
       iusRefClk    <= usRefClk;
       iusRefClkGt  <= usRefClkGt;
       itimingRxP   <= idsRxP(0)(0);
       itimingRxN   <= idsRxN(0)(0);
       idsTxP(0)(0) <= itimingTxP;
       idsTxN(0)(0) <= itimingTxN;
       U_GTH : entity surf.Gthe3ChannelDummy
         port map ( refClk   => dsClkBuf(0),
                    gtRxP(0) => timingRxP,
                    gtRxN(0) => timingRxN,
                    gtTxP(0) => timingTxP,
                    gtTxN(0) => timingTxN );
     end generate GEN_CU_RX;
   end generate;

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
         sAxiWriteMasters(0) => regWriteMaster,
         sAxiWriteSlaves(0)  => regWriteSlave,
         sAxiReadMasters(0)  => regReadMaster,
         sAxiReadSlaves (0)  => regReadSlave,
         mAxiWriteMasters    => axilWriteMasters,
         mAxiWriteSlaves     => axilWriteSlaves,
         mAxiReadMasters     => axilReadMasters,
         mAxiReadSlaves      => axilReadSlaves);

   U_SyncDbg : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => dbgChan'length)
      port map (
         clk     => recTimingClk,
         dataIn  => dbgChan,
         dataOut => dbgChanS);

   GEN_RINGD : for i in 0 to NUM_FP_LINKS_C-1 generate
      U_Sync : entity surf.SynchronizerFifo
         generic map (
            TPD_G        => TPD_G,
            DATA_WIDTH_G => 19)
         port map (
            wr_clk            => dsRxClk(i),
            din(18)           => dsRxErr (i),
            din(17 downto 16) => dsRxDataK(i),
            din(15 downto 0)  => dsRxData (i),
            rd_clk            => recTimingClk,
            valid             => ringDataV(i),
            dout              => ringDataI(i));
   end generate;

   process (recTimingClk) is
      variable iLink : integer;
   begin
      if rising_edge(recTimingClk) then
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
         dataClk         => recTimingClk,
         dataRst         => '0',
         dataValid       => '1',
         dataValue       => ringData,
         axilClk         => regClk,
         axilRst         => regRst,
         axilReadMaster  => axilReadMasters (RING_INDEX_C),
         axilReadSlave   => axilReadSlaves  (RING_INDEX_C),
         axilWriteMaster => axilWriteMasters(RING_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (RING_INDEX_C));

   U_FpgaClkRet : BUFG_GT
      port map (
         O       => fpgaclk_ret,
         CE      => '1',
         CEMASK  => '1',
         CLR     => '0',
         CLRMASK => '1',
         DIV     => "000",              -- Divide-by-1
         I       => dsTxOutClk(0));

   pllLocked <= not (pllStatus(1).lol or pllStatus(1).los) &
                not (pllStatus(0).lol or pllStatus(0).los);

   U_Phase : entity l2si.XpmPhase
      generic map (
         TPD_G => TPD_G)
      port map (
         clkIn           => fpgaclk_ret,
         syncClk         => cuRecClk,
         syncRst         => recTimingLkN,
         syncIn          => cuSync,
         axilClk         => regClk,
         axilRst         => regRst,
         axilReadMaster  => axilReadMasters (PHAS_INDEX_C),
         axilReadSlave   => axilReadSlaves  (PHAS_INDEX_C),
         axilWriteMaster => axilWriteMasters(PHAS_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (PHAS_INDEX_C));

   U_PLLCLK : entity l2si.XpmPllClk
      generic map (
         TPD_G   => TPD_G,
         ASYNC_G => CU_ASYNC_G )
      port map (
         clkIn           => recTimingClk,
         rstIn           => recTimingRst,
         locked          => pllLocked,
         clkOutP         => fpgaclk_P,
         clkOutN         => fpgaclk_N,
         clkRet          => fpgaclk_ret,
         syncRst         => recTimingLkN,
         syncIn          => cuSync,
         axilClk         => regClk,
         axilRst         => regRst,
         axilReadMaster  => axilReadMasters (MMCM_INDEX_C),
         axilReadSlave   => axilReadSlaves  (MMCM_INDEX_C),
         axilWriteMaster => axilWriteMasters(MMCM_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (MMCM_INDEX_C) );

   GEN_PLL : for i in 0 to 1 generate
      U_Pll : entity l2si.XpmPll
         generic map (
            TPD_G => TPD_G)
         port map (
            config => xpmConfig.pll(i),
            status => pllStatus (i),
            frqTbl => frqTbl (i),
            frqSel => frqSel (i),
            bwSel  => bwSel (i),
            inc    => inc (i),
            dec    => dec (i),
            sfOut  => sfOut (i),
            rate   => rate (i),
            bypass => bypass (i),
            pllRst => pllRst (i),
            lol    => lol (i),
            los    => los (i));
   end generate;

   U_Application : entity l2si.XpmApp
      generic map (
         TPD_G           => TPD_G,
         NUM_DS_LINKS_G  => NUM_FP_LINKS_C,
         NUM_DDC_G       => NUM_DDC_C,
         NUM_SEQ_G       => NUM_SEQ_C,
         AXIL_BASEADDR_G => AXI_XBAR_CONFIG_C(APP_INDEX_C).baseAddr)
      port map (
         -----------------------
         -- Application Ports --
         -----------------------
         -- -- AMC's DS Ports
         dsLinkStatus    => dsLinkStatus(NUM_FP_LINKS_C-1 downto 0),
         dsRxData        => dsRxData    (NUM_FP_LINKS_C-1 downto 0),
         dsRxDataK       => dsRxDataK   (NUM_FP_LINKS_C-1 downto 0),
         dsTxData        => dsTxData    (NUM_FP_LINKS_C-1 downto 0),
         dsTxDataK       => dsTxDataK   (NUM_FP_LINKS_C-1 downto 0),
         dsRxClk         => dsRxClk     (NUM_FP_LINKS_C-1 downto 0),
         dsRxRst         => dsRxRst     (NUM_FP_LINKS_C-1 downto 0),
         dsRxErr         => dsRxErr     (NUM_FP_LINKS_C-1 downto 0),
         ----------------------
         -- Top Level Interface
         ----------------------
         regclk          => regClk,
         regrst          => regRst,
         update          => regUpdate,
         status          => xpmStatus,
         patternCfg      => patternCfg,
         pattern         => pattern,
         common          => common,
         config          => xpmConfig,
         axilReadMaster  => axilReadMasters (APP_INDEX_C),
         axilReadSlave   => axilReadSlaves  (APP_INDEX_C),
         axilWriteMaster => axilWriteMasters(APP_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (APP_INDEX_C),
         groupLinkClear  => groupLinkClear,
         seqRestart      => seqRestart,
         seqDisable      => seqDisable,
         -- Async Notification
         obAppMaster     => seqMaster,
         obAppSlave      => seqSlave,
         -- Timing Interface (timingClk domain) 
         timingClk       => recTimingClk,
         timingRst       => recTimingRst,
--         timingIn        => recTimingData,
         timingStream    => recStream,
         timingFbClk     => timingPhyClk,
         timingFbRst     => '0',
         timingFbId      => timingPhyId,
         timingFb        => timingPhy,
         seqCountRst     => seqCountRst,
         seqCount        => seqCount,
         timeStamp       => timeStamp );

   U_MasterMux : entity surf.AxiStreamMux
     generic map ( NUM_SLAVES_G => 3 )
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
                
   U_Core : entity l2si.XpmCore
      generic map (
         TPD_G               => TPD_G,
         BUILD_INFO_G        => BUILD_INFO_G,
         USE_XTPG_G          => USE_XTPG_G,
         US_RX_ENABLE_INIT_G => US_RX_ENABLE_INIT_G,
         CU_RX_ENABLE_INIT_G => CU_RX_ENABLE_INIT_G,
         CU_ASYNC_G          => CU_ASYNC_G,
         UED_MODE_G          => UED_MODE_G )
     port map (
         ----------------------
         -- Top Level Interface
         ----------------------
         -- AXI-Lite Interface (regClk domain)
         regClk           => regClk,
         regRst           => regRst,
         regReadMaster    => regReadMaster,
         regReadSlave     => regReadSlave,
         regWriteMaster   => regWriteMaster,
         regWriteSlave    => regWriteSlave,
         -- Streaming input (regClk domain)
         ibDebugMaster    => ibDebugMaster,
         ibDebugSlave     => ibDebugSlave,
         -- Async Notification
         obDebugMaster    => obDebugMaster,
         obDebugSlave     => obDebugSlave,
         -- Timing Interface (timingClk domain)
--         timingData        => recTimingDataI,
         recStream        => recStream,
         timingPhy        => timingPhy,
         -- Reference Clocks and Resets
         timingPhyClk     => timingPhyClk,
         recTimingClk     => recTimingClk,
         recTimingLkN     => recTimingLkN,
         recTimingRst     => recTimingRst,
         ref125MHzClk     => ref125MHzClk,
         ref125MHzRst     => ref125MHzRst,
         ref156MHzClk     => ref156MHzClk,
         ref156MHzRst     => ref156MHzRst,
         ref312MHzClk     => open,
         ref312MHzRst     => open,
         ref625MHzClk     => open,
         ref625MHzRst     => open,
         gthFabClk        => open,
         ----------------
         -- Core Ports --
         ----------------   
         -- Common Fabricate Clock
         fabClkP          => fabClkP,
         fabClkN          => fabClkN,
         -- ETH Ports
         ethRxP           => ethRxP,
         ethRxN           => ethRxN,
         ethTxP           => ethTxP,
         ethTxN           => ethTxN,
         ethClkP          => ethClkP,
         ethClkN          => ethClkN,
         ipAddr           => ipAddr,
         -- LCLS-I Timing Ports
         cuRxEnable       => '0',
         cuRecClk         => cuRecClk,
         cuRecFiducial    => cuRecFiducial,
         cuSync           => cuSync,
         -- LCLS-II Timing Ports
         usRxEnable       => usRxEnable,
         usRxP            => iusRxP,
         usRxN            => iusRxN,
         usTxP            => iusTxP,
         usTxN            => iusTxN,
         usRefClk         => iusRefClk,
         usRefClkGt       => iusRefClkGt,
         timingRecClkOutP => timingRecClkOutP,  -- to AMC PLL
         timingRecClkOutN => timingRecClkOutN,
         --
         timingClkScl     => timingClkScl,
         timingClkSda     => timingClkSda,
         -- Timing Reference (standalone system only)
         timingClkSel     => timingClkSel,
         timingRefClkGt   => timingRefClkGt,
         timingRefClk     => timingRefClk,
         timingRxP        => itimingRxP,
         timingRxN        => itimingRxN,
         timingTxP        => itimingTxP,
         timingTxN        => itimingTxN,
         -- Crossbar Ports
         xBarSin          => xBarSin,
         xBarSout         => xBarSout,
         xBarConfig       => xBarConfig,
         xBarLoad         => xBarLoad,
         -- IPMC Ports
         ipmcScl          => ipmcScl,
         ipmcSda          => ipmcSda,
         -- AMC SMBus Ports
         hsrScl           => hsrScl,
         hsrSda           => hsrSda,
         amcScl           => amcScl,
         amcSda           => amcSda,
         -- Configuration PROM Ports
         calScl           => calScl,
         calSda           => calSda,
         -- No DDR
         ddrScl           => ddrScl,
         ddrSda           => ddrSda,
         -- SYSMON Ports
         vPIn             => vPIn,
         vNIn             => vNIn);

   U_Reg : entity l2si.XpmReg
      generic map(
         TPD_G               => TPD_G,
         NUM_DS_LINKS_G      => NUM_FP_LINKS_C,
         US_RX_ENABLE_INIT_G => US_RX_ENABLE_INIT_G,
         CU_RX_ENABLE_INIT_G => CU_RX_ENABLE_INIT_G,
         STA_INTERVAL_C      => ite(UED_MODE_G, 500000, 910000),
         DSCLK_119MHZ_G      => (UED_MODE_G),
         NUM_SEQ_G           => NUM_SEQ_C,
         NUM_DDC_G           => NUM_DDC_C )
      port map (
         axilClk         => regClk,
         axilRst         => regRst,
         axilUpdate      => regUpdate,
         axilReadMaster  => axilReadMasters (REG_INDEX_C),
         axilReadSlave   => axilReadSlaves  (REG_INDEX_C),
         axilWriteMaster => axilWriteMasters(REG_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (REG_INDEX_C),
         seqRestart      => seqRestart,
         seqDisable      => seqDisable,
         groupLinkClear  => groupLinkClear,
         -- Streaming input (regClk domain)
         ibDebugMaster   => ibDebugMaster,
         ibDebugSlave    => ibDebugSlave,
         obDebugMaster   => stepMaster,
         obDebugSlave    => stepSlave,
         obMonitorMaster => monMaster,
         obMonitorSlave  => monSlave,
         staClk          => recTimingClk,
         pllStatus       => pllStatus,
         status          => xpmStatus,
         pattern         => pattern,
         patternCfg      => patternCfg,
         monClk(0)       => cuRecClk,
         monClk(1)       => timingPhyClk,
         monClk(2)       => recTimingClk,
         monClk(3)       => iusRefClk,
         monLatch        => seqCountRst,
         seqCount        => seqCount,
         timeStamp       => timeStamp,
         config          => xpmConfig,
         common          => common,
         usRxEnable      => usRxEnable,
         cuRxEnable      => cuRxEnable,
         dbgChan         => dbgChan );

   GEN_AMC_MGT : for i in 0 to 1 generate
    
      txClkRst(i) <= not pllLocked(i) or recTimingRst;

      U_Rcvr : entity l2si.XpmGthUltrascaleWrapper
         generic map (
            GTGCLKRX   => false,
            NLINKS_G   => AMC_DS_LINKS_C(i),
            USE_IBUFDS => true,
            AXIL_BASE_ADDR_G => AXI_XBAR_CONFIG_C(AMC_GTH_INDEX_C+i).baseAddr
            )
         port map (
            stableClk => regClk,
            gtTxP     => idsTxP (i)(AMC_DS_PORTN_C(i) downto AMC_DS_PORT0_C(i)),
            gtTxN     => idsTxN (i)(AMC_DS_PORTN_C(i) downto AMC_DS_PORT0_C(i)),
            gtRxP     => idsRxP (i)(AMC_DS_PORTN_C(i) downto AMC_DS_PORT0_C(i)),
            gtRxN     => idsRxN (i)(AMC_DS_PORTN_C(i) downto AMC_DS_PORT0_C(i)),
            devClkP   => dsClockP (i),
            devClkN   => dsClockN (i),
            devClkOut => dsClkBuf (i),
            devClkBuf => dsTxOutClk(7*i),
            txData    => dsTxData  (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            txDataK   => dsTxDataK (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            rxData    => dsRxData  (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            rxDataK   => dsRxDataK (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            rxClk     => dsRxClk   (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            rxRst     => dsRxRst   (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            rxErr     => dsRxErr   (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
--                 txOutClk        => dsTxOutClk(7*i+6 downto 7*i),
            txClk     => open,
            txClkIn   => recTimingClk,
            txClkRst  => txClkRst(i),
            config    => xpmConfig.dsLink(AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
            status    => dsLinkStatus    (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),

            axilRst          => regRst,
            axilReadMaster   => axilReadMasters (AMC_GTH_INDEX_C+i),
            axilReadSlave    => axilReadSlaves  (AMC_GTH_INDEX_C+i),
            axilWriteMaster  => axilWriteMasters(AMC_GTH_INDEX_C+i),
            axilWriteSlave   => axilWriteSlaves (AMC_GTH_INDEX_C+i)
        );
   end generate;

   --
   --  Need to clock unconnected channels for deadtime mask to work properly
   --
   GEN_RX0: if not USE_RTM_G generate
     dsRxClk(0)      <= recTimingClk;
     dsRxRst(0)      <= recTimingRst;
   end generate GEN_RX0;
  
   U_SyncPaddrTx : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => xpmConfig.paddr'length-4)
      port map (
         clk     => timingPhyClk,
         dataIn  => xpmConfig.paddr(xpmConfig.paddr'left-4 downto 0),
         dataOut => timingPhyId(timingPhyId'left downto 4) );
   timingPhyId(3 downto 0) <= x"F";
   
   --U_CuRxFiducial : OBUFDS
   --  port map ( I  => cuRxFiducial,
   --             O  => rtmLsP(32),
   --             OB => rtmLsN(32) );

   --U_CuSync : OBUFDS
   --  port map ( I  => cuSync,
   --             O  => rtmLsP(33),
   --             OB => rtmLsN(33) );

   --U_CuRxClk : entity surf.ClkOutBufDiff
   --  generic map ( XIL_DEVICE_G => "ULTRASCALE" )
   --  port map ( clkIn   => cuRxClk,
   --             clkOutP => rtmLsP(34),
   --             clkOutN => rtmLsN(34) );

   --U_RecTimingClk : entity surf.ClkOutBufDiff
   --  generic map ( XIL_DEVICE_G => "ULTRASCALE" )
   --  port map ( clkIn   => recTimingClk,
   --             clkOutP => rtmLsP(35),
   --             clkOutN => rtmLsN(35) );

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

