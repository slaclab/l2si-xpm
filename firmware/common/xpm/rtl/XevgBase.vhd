-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XevgBase.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2021-10-14
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

--library amc_carrier_core;
--use amc_carrier_core.AmcCarrierPkg.all;

library l2si;

entity XevgBase is
  generic (
    TPD_G               : time    := 1 ns;
    BUILD_INFO_G        : BuildInfoType;
    USE_RTM_G           : boolean := true );  -- Get LCLS2 input from RTM vs AMC
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
    timingRxP        : in    sl;      -- LCLS-2 Timing Input
    timingRxN        : in    sl;
    timingRefClkInP  : in    sl;
    timingRefClkInN  : in    sl;
    --
    timingRecClkOutP : out   sl;
    timingRecClkOutN : out   sl;
    timingClkSel     : out   sl;
    timingClkScl     : inout sl;      -- jitter cleaner (unused)
    timingClkSda     : inout sl;
    fpgaclk_P        : out   slv(3 downto 0);
    fpgaclk_N        : out   slv(3 downto 0);
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
end XevgBase;

architecture top_level of XevgBase is

  -- AmcCarrierCore Configuration Constants
  constant DIAGNOSTIC_SIZE_C   : positive            := 1;
  constant DIAGNOSTIC_CONFIG_C : AxiStreamConfigType := ssiAxiStreamConfig(4);

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
  signal timingPhyRst : sl;
  signal recTimingClk : sl;
  signal recTimingRst : sl;
  signal recTimingLkN : sl;
  signal ref125MHzClk : sl;
  signal ref125MHzRst : sl;
  signal ref156MHzClk : sl;
  signal ref156MHzRst : sl;

  constant NUM_DS_LINKS_C : integer := ite(USE_RTM_G,14,13);
  constant NUM_FP_LINKS_C : integer := 14;
  constant NUM_BP_LINKS_C : integer := 6;

  signal xpmConfig : XpmConfigType;
  signal xpmStatus : XpmStatusType;
  signal bpStatus  : XpmBpLinkStatusArray(NUM_BP_LINKS_C downto 0);
  signal pllStatus : XpmPllStatusArray (1 downto 0);
  signal pllLocked : slv(1 downto 0);

  signal dsClockP : slv(1 downto 0);
  signal dsClockN : slv(1 downto 0);
  signal idsRxP   : Slv7Array(1 downto 0);
  signal idsRxN   : Slv7Array(1 downto 0);
  signal idsTxP   : Slv7Array(1 downto 0);
  signal idsTxN   : Slv7Array(1 downto 0);

  signal dsTx         : slv(15 downto 0);
  signal dsTxK        : slv( 1 downto 0);
  signal dsLinkStatus : XpmLinkStatusArray(NUM_FP_LINKS_C-1 downto 0);
  signal dsTxData     : Slv16Array(NUM_FP_LINKS_C-1 downto 0);
  signal dsTxDataK    : Slv2Array (NUM_FP_LINKS_C-1 downto 0);
  signal dsRxData     : Slv16Array(NUM_FP_LINKS_C-1 downto 0);
  signal dsRxDataK    : Slv2Array (NUM_FP_LINKS_C-1 downto 0);
  signal dsRxClk      : slv (NUM_FP_LINKS_C-1 downto 0);
  signal dsRxRst      : slv (NUM_FP_LINKS_C-1 downto 0);
  signal dsRxErr      : slv (NUM_FP_LINKS_C-1 downto 0);
  signal dsTxOutClk   : slv (NUM_FP_LINKS_C-1 downto 0);

  signal bpRxLinkUp   : slv (NUM_BP_LINKS_C-1 downto 0);
  signal bpRxLinkFull : Slv16Array (NUM_BP_LINKS_C-1 downto 0);
  signal bpTxData     : Slv16Array(0 downto 0);
  signal bpTxDataK    : Slv2Array (0 downto 0);

  constant AXI_XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(5 downto 0) := genAxiLiteConfig(6, x"80000000", 23, 16);

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

  signal bpMonClk : sl;
  signal ipAddr   : slv(31 downto 0);

  signal usRxEnable : sl;
  signal groupLinkClear         : slv(XPM_PARTITIONS_C-1 downto 0);

  -- Timing Interface (timingClk domain)
  signal timingPhy : TimingPhyType;

  signal timingRefClk, itimingRefClk, timingRefClkGt : sl;
  signal usRefClk, usRefClkO, usRefClkGt : sl;
  signal usSync : sl;
  
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

  signal tmpReg : slv(31 downto 0) := x"DEADBEEF";

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
      ODIV2 => itimingRefClk,   -- 186 MHz
      O     => timingRefClkGt); -- 371 MHz

  U_BUFG_GT : BUFG_GT
    port map ( O       => timingRefClk,
               CE      => '1',
               CEMASK  => '1',
               CLR     => '0',
               CLRMASK => '1',
               DIV     => "000",           -- Divide-by-1
               I       => itimingRefClk );

  iusRxP       <= idsRxP(0)(0);
  iusRxN       <= idsRxN(0)(0);
  idsTxP(0)(0) <= iusTxP;
  idsTxN(0)(0) <= iusTxN;
  iusRefClk    <= timingRefClk;
  iusRefClkGt  <= timingRefClkGt;
  
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
      syncClk         => recTimingClk,
      syncRst         => recTimingLkN,
      syncIn          => usSync,
      axilClk         => regClk,
      axilRst         => regRst,
      axilReadMaster  => axilReadMasters(5),
      axilReadSlave   => axilReadSlaves(5),
      axilWriteMaster => axilWriteMasters(5),
      axilWriteSlave  => axilWriteSlaves(5));

  U_PLLCLK : entity l2si.XpmPllClk
    generic map (
      TPD_G   => TPD_G,
      MODE_G  => "119M",
      ASYNC_G => false )
    port map (
      clkIn           => recTimingClk,
      rstIn           => recTimingRst,
      locked          => pllLocked,
      clkOutP         => fpgaclk_P,
      clkOutN         => fpgaclk_N,
      clkRet          => fpgaclk_ret,
      syncRst         => recTimingLkN,
      syncIn          => usSync,
      axilClk         => regClk,
      axilRst         => regRst,
      axilReadMaster  => axilReadMasters (4),
      axilReadSlave   => axilReadSlaves (4),
      axilWriteMaster => axilWriteMasters(4),
      axilWriteSlave  => axilWriteSlaves (4));

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

  U_GTH : entity surf.Gthe3ChannelDummy
    port map ( refClk   => dsClkBuf(1),
               gtRxP(0) => timingRxP,
               gtRxN(0) => timingRxN,
               gtTxP(0) => timingTxP,
               gtTxN(0) => timingTxN );

  GEN_DSLINK : for i in 0 to NUM_DS_LINKS_C-1 generate
    dsTxData (i) <= timingPhy.data;
    dsTxDataK(i) <= timingPhy.dataK;
  end generate;

  U_Core : entity l2si.XevgCore
    generic map (
      TPD_G               => TPD_G,
      BUILD_INFO_G        => BUILD_INFO_G )
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
      timingPhyClk     => timingPhyClk,
      timingPhyRst     => timingPhyRst,
      timingPhy        => timingPhy,
      -- Reference Clocks and Resets
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
      -- LCLS-II Timing Ports
      usRxEnable       => usRxEnable,
      usRxP            => iusRxP,
      usRxN            => iusRxN,
      usTxP            => iusTxP,
      usTxN            => iusTxN,
      usRefClk         => iusRefClk,
      usRefClkGt       => iusRefClkGt,
      usRxSync         => usSync,
      timingRecClkOutP => timingRecClkOutP,  -- to AMC PLL
      timingRecClkOutN => timingRecClkOutN,
      --
      timingClkScl     => timingClkScl,
      timingClkSda     => timingClkSda,
      -- Timing Reference (standalone system only)
      timingClkSel     => timingClkSel,
      timingRefClkGt   => timingRefClkGt,
      timingRefClk     => timingRefClk,
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
        config    => xpmConfig.dsLink(AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)),
        status    => dsLinkStatus    (AMC_DS_LAST_C(i) downto AMC_DS_FIRST_C(i)));
  end generate;

  U_AXI_EMPTY : entity surf.AxiLiteRegs
    port map (
      axiClk         => regClk,
      axiClkRst      => regRst,
      axiReadMaster  => axilReadMasters(3),
      axiReadSlave   => axilReadSlaves (3),
      axiWriteMaster => axilWriteMasters(3),
      axiWriteSlave  => axilWriteSlaves (3),
      writeRegister(0) => tmpReg,
      readRegister (0) => tmpReg );
  
end top_level;

