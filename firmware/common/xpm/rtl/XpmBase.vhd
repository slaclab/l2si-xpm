-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmBase.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2020-02-12
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Firmware Target's Top Level
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
use l2si_core.XpmMiniPkg.all;

library amc_carrier_core;
use amc_carrier_core.AmcCarrierPkg.all;

library l2si;

entity XpmBase is
   generic (
      TPD_G               : time    := 1 ns;
      BUILD_INFO_G        : BuildInfoType;
      USE_XTPG_G          : boolean := false;
      US_RX_ENABLE_INIT_G : boolean := true;
      CU_RX_ENABLE_INIT_G : boolean := false;
      GEN_BP_G            : boolean := false);
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

   -- AXI-Lite Interface (appClk domain)
   signal regClk         : sl;
   signal regRst         : sl;
   signal regUpdate      : slv(XPM_PARTITIONS_C-1 downto 0);
   signal regReadMaster  : AxiLiteReadMasterType;
   signal regReadSlave   : AxiLiteReadSlaveType;
   signal regWriteMaster : AxiLiteWriteMasterType;
   signal regWriteSlave  : AxiLiteWriteSlaveType;

   -- Timing Interface (timingClk domain)
   --signal recTimingDataI : TimingRxType;
   --signal recTimingData  : TimingRxType;
   signal recStream : XpmMiniStreamType;
   signal timingPhy : TimingPhyType;

   -- Reference Clocks and Resets
   signal timingPhyClk : sl;
   signal recTimingClk : sl;
   signal recTimingRst : sl;
   signal ref125MHzClk : sl;
   signal ref125MHzRst : sl;
   signal ref156MHzClk : sl;
   signal ref156MHzRst : sl;

   constant NUM_DS_LINKS_C : integer := 14;
   constant NUM_BP_LINKS_C : integer := 6;

   signal xpmConfig : XpmConfigType;
   signal xpmStatus : XpmStatusType;
   signal bpStatus  : XpmBpLinkStatusArray(NUM_BP_LINKS_C downto 0);
   signal pllStatus : XpmPllStatusArray (1 downto 0);
   signal pllLocked : sl;

   signal dsClockP : slv(1 downto 0);
   signal dsClockN : slv(1 downto 0);
   signal idsRxP   : Slv7Array(1 downto 0);
   signal idsRxN   : Slv7Array(1 downto 0);
   signal idsTxP   : Slv7Array(1 downto 0);
   signal idsTxN   : Slv7Array(1 downto 0);

   signal dsLinkStatus : XpmLinkStatusArray(NUM_DS_LINKS_C-1 downto 0);
   signal dsTxData     : Slv16Array(NUM_DS_LINKS_C-1 downto 0);
   signal dsTxDataK    : Slv2Array (NUM_DS_LINKS_C-1 downto 0);
   signal dsRxData     : Slv16Array(NUM_DS_LINKS_C-1 downto 0);
   signal dsRxDataK    : Slv2Array (NUM_DS_LINKS_C-1 downto 0);
   signal dsRxClk      : slv (NUM_DS_LINKS_C-1 downto 0);
   signal dsRxRst      : slv (NUM_DS_LINKS_C-1 downto 0);
   signal dsRxErr      : slv (NUM_DS_LINKS_C-1 downto 0);
   signal dsTxOutClk   : slv (NUM_DS_LINKS_C-1 downto 0);

   signal bpRxLinkUp   : slv (NUM_BP_LINKS_C-1 downto 0);
   signal bpRxLinkFull : Slv16Array (NUM_BP_LINKS_C-1 downto 0);
   signal bpTxData     : Slv16Array(0 downto 0);
   signal bpTxDataK    : Slv2Array (0 downto 0);

   signal dbgChan   : slv(4 downto 0);
   signal dbgChanS  : slv(4 downto 0);
   signal ringData  : slv(19 downto 0);
   signal ringDataI : Slv19Array(NUM_DS_LINKS_C-1 downto 0);
   signal ringDataV : slv (NUM_DS_LINKS_C-1 downto 0);

   constant AXI_XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(5 downto 0) := genAxiLiteConfig(6, x"80000000", 23, 16);

   signal axilReadMasters  : AxiLiteReadMasterArray (AXI_XBAR_CONFIG_C'range);
   signal axilReadSlaves   : AxiLiteReadSlaveArray (AXI_XBAR_CONFIG_C'range);
   signal axilWriteMasters : AxiLiteWriteMasterArray(AXI_XBAR_CONFIG_C'range);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray (AXI_XBAR_CONFIG_C'range);

   signal ibDebugMaster : AxiStreamMasterType;
   signal ibDebugSlave  : AxiStreamSlaveType;
   signal obDebugMaster : AxiStreamMasterType;
   signal obDebugSlave  : AxiStreamSlaveType;

   signal dsClkBuf    : slv(1 downto 0);
   signal fpgaclk_ret : sl;

   signal bpMonClk : sl;
   signal ipAddr   : slv(31 downto 0);

--   signal cuRxFiducial, cuSync : sl;
   signal cuSync        : sl;
   signal cuLocked      : sl;
   signal cuRecClk      : sl;
   signal cuRecFiducial : sl;

   signal usRxEnable, cuRxEnable : sl;
   signal groupLinkClear         : slv(XPM_PARTITIONS_C-1 downto 0);

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
         axilReadMaster  => axilReadMasters (1),
         axilReadSlave   => axilReadSlaves (1),
         axilWriteMaster => axilWriteMasters(1),
         axilWriteSlave  => axilWriteSlaves (1));

   U_FpgaClkRet : BUFG_GT
      port map (
         O       => fpgaclk_ret,
         CE      => '1',
         CEMASK  => '1',
         CLR     => '0',
         CLRMASK => '1',
         DIV     => "000",              -- Divide-by-1
         I       => dsTxOutClk(0));

   pllLocked <= not (pllStatus(0).lol or pllStatus(1).lol or
                     pllStatus(0).los or pllStatus(1).los);

   U_Phase : entity l2si.XpmPhase
      generic map (
         TPD_G => TPD_G)
      port map (
         clkIn           => fpgaclk_ret,
         syncClk         => cuRecClk,
         syncRst         => cuLocked,
         syncIn          => cuSync,
         axilClk         => regClk,
         axilRst         => regRst,
         axilReadMaster  => axilReadMasters(5),
         axilReadSlave   => axilReadSlaves(5),
         axilWriteMaster => axilWriteMasters(5),
         axilWriteSlave  => axilWriteSlaves(5));

   U_PLLCLK : entity l2si.XpmPllClk
      generic map (
         TPD_G => TPD_G)
      port map (
         clkIn           => recTimingClk,
         rstIn           => recTimingRst,
         locked          => pllLocked,
         clkOutP         => fpgaclk_P,
         clkOutN         => fpgaclk_N,
         clkRet          => fpgaclk_ret,
         syncRst         => cuLocked,
         syncIn          => cuSync,
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

   U_Application : entity l2si_core.XpmApp
      generic map (
         TPD_G           => TPD_G,
         NUM_DS_LINKS_G  => NUM_DS_LINKS_C,
         NUM_BP_LINKS_G  => NUM_BP_LINKS_C,
         AXIL_BASEADDR_G => AXI_XBAR_CONFIG_C(2).baseAddr)
      port map (
         -----------------------
         -- Application Ports --
         -----------------------
         -- -- AMC's DS Ports
         dsLinkStatus    => dsLinkStatus,
         dsRxData        => dsRxData,
         dsRxDataK       => dsRxDataK,
         dsTxData        => dsTxData,
         dsTxDataK       => dsTxDataK,
         dsRxClk         => dsRxClk,
         dsRxRst         => dsRxRst,
         dsRxErr         => dsRxErr,
         --  BP DS Ports
         bpTxData        => bpTxData (0),
         bpTxDataK       => bpTxDataK(0),
         bpStatus        => bpStatus,
         bpRxLinkFull    => bpRxLinkFull,
         ----------------------
         -- Top Level Interface
         ----------------------
         regclk          => regClk,
         regrst          => regRst,
         update          => regUpdate,
         status          => xpmStatus,
         config          => xpmConfig,
         axilReadMaster  => axilReadMasters(2),
         axilReadSlave   => axilReadSlaves(2),
         axilWriteMaster => axilWriteMasters(2),
         axilWriteSlave  => axilWriteSlaves(2),
         groupLinkClear  => groupLinkClear,
         -- Async Notification
         obAppMaster     => obDebugMaster,
         obAppSlave      => obDebugSlave,
         -- Timing Interface (timingClk domain) 
         timingClk       => recTimingClk,
         timingRst       => recTimingRst,
--         timingIn        => recTimingData,
         timingStream    => recStream,
         timingFbClk     => timingPhyClk,
         timingFbRst     => '0',
         timingFbId      => ipAddr,     --xpmTimingFbId(ipAddr),
         timingFb        => timingPhy);

   GEN_BP : if GEN_BP_G generate
      U_Backplane : entity l2si.XpmBp
         generic map (
            TPD_G          => TPD_G,
            NUM_BP_LINKS_G => NUM_BP_LINKS_C)
         port map (
            ----------------------
            -- Top Level Interface
            ----------------------
            ref125MHzClk => ref125MHzClk,
            ref125MHzRst => ref125MHzRst,
            rxFull       => bpRxLinkFull,
            config       => xpmConfig.bpLink(NUM_BP_LINKS_C downto 1),
            status       => bpStatus(NUM_BP_LINKS_C downto 1),
            monClk       => bpMonClk,
            --
            timingClk    => recTimingClk,
            timingRst    => recTimingRst,
--      timingBus       => recTimingBus,
            timingBus    => TIMING_BUS_INIT_C,
            ----------------
            -- Core Ports --
            ----------------
            -- Backplane MPS Ports
            bpClkIn      => bpClkIn,
            bpClkOut     => bpClkOut,
            bpBusRxP     => bpBusRxP(NUM_BP_LINKS_C downto 1),
            bpBusRxN     => bpBusRxN(NUM_BP_LINKS_C downto 1));

      GEN_BPRX : for i in NUM_BP_LINKS_C+1 to 14 generate
         U_RX : IBUFDS
            port map (
               I  => bpBusRxP(i),
               IB => bpBusRxN(i),
               O  => open);
      end generate;

   end generate;

   NOGEN_BP : if not GEN_BP_G generate
      bpStatus(NUM_BP_LINKS_C downto 1) <= (others => XPM_BP_LINK_STATUS_INIT_C);
      bpMonClk                          <= '0';
      bpClkOut                          <= '0';

      GEN_BPRX : for i in 1 to 14 generate
         U_RX : IBUFDS
            port map (
               I  => bpBusRxP(i),
               IB => bpBusRxN(i),
               O  => open);
      end generate;
   end generate;

   U_Core : entity l2si.XpmCore
      generic map (
         TPD_G               => TPD_G,
         BUILD_INFO_G        => BUILD_INFO_G,
         USE_XTPG_G          => USE_XTPG_G,
         US_RX_ENABLE_INIT_G => US_RX_ENABLE_INIT_G,
         CU_RX_ENABLE_INIT_G => CU_RX_ENABLE_INIT_G)
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
         timingDevClk     => dsClkBuf(0),
         bpTxData         => bpTxData(0),
         bpTxDataK        => bpTxDataK(0),
         cuSync           => cuSync,
         cuLocked         => cuLocked,
         -- LCLS-II Timing Ports
         usRxEnable       => usRxEnable,
         usRxP            => usRxP,
         usRxN            => usRxN,
         usTxP            => usTxP,
         usTxN            => usTxN,
         usRefClkP        => usClkP,            -- GEN0 REF
         usRefClkN        => usClkN,
         timingRecClkOutP => timingRecClkOutP,  -- to AMC PLL
         timingRecClkOutN => timingRecClkOutN,
         --
         timingRefClkOut  => open,
         timingClkScl     => timingClkScl,
         timingClkSda     => timingClkSda,
         -- Timing Reference (standalone system only)
         timingClkSel     => timingClkSel,
         timingRefClkInP  => timingRefClkInP,
         timingRefClkInN  => timingRefClkInN,
         timingRxP        => timingRxP,
         timingRxN        => timingRxN,
         timingTxP        => timingTxP,
         timingTxN        => timingTxN,
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
         NUM_DS_LINKS_G      => NUM_DS_LINKS_C,
         NUM_BP_LINKS_G      => NUM_BP_LINKS_C,
         US_RX_ENABLE_INIT_G => US_RX_ENABLE_INIT_G,
         CU_RX_ENABLE_INIT_G => CU_RX_ENABLE_INIT_G)
      port map (
         axilClk         => regClk,
         axilRst         => regRst,
         axilUpdate      => regUpdate,
         axilReadMaster  => axilReadMasters(0),
         axilReadSlave   => axilReadSlaves(0),
         axilWriteMaster => axilWriteMasters(0),
         axilWriteSlave  => axilWriteSlaves(0),
         groupLinkClear  => groupLinkClear,
         -- Streaming input (regClk domain)
         ibDebugMaster   => ibDebugMaster,
         ibDebugSlave    => ibDebugSlave,
         staClk          => recTimingClk,
         pllStatus       => pllStatus,
         status          => xpmStatus,
         monClk(0)       => bpMonClk,
         monClk(1)       => timingPhyClk,
         monClk(2)       => recTimingClk,
         monClk(3)       => bpMonClk,
         config          => xpmConfig,
         usRxEnable      => usRxEnable,
         cuRxEnable      => cuRxEnable,
         dbgChan         => dbgChan);

   GEN_AMC_MGT : for i in 0 to 1 generate
      U_Rcvr : entity l2si.XpmGthUltrascaleWrapper
         generic map (
            GTGCLKRX   => false,
            NLINKS_G   => 7,
            USE_IBUFDS => true)
         port map (
            stableClk => regClk,
            gtTxP     => idsTxP (i),
            gtTxN     => idsTxN (i),
            gtRxP     => idsRxP (i),
            gtRxN     => idsRxN (i),
            devClkP   => dsClockP (i),
            devClkN   => dsClockN (i),
            devClkOut => dsClkBuf (i),
            devClkBuf => dsTxOutClk(7*i),
            txData    => dsTxData (7*i+6 downto 7*i),
            txDataK   => dsTxDataK (7*i+6 downto 7*i),
            rxData    => dsRxData (7*i+6 downto 7*i),
            rxDataK   => dsRxDataK (7*i+6 downto 7*i),
            rxClk     => dsRxClk (7*i+6 downto 7*i),
            rxRst     => dsRxRst (7*i+6 downto 7*i),
            rxErr     => dsRxErr (7*i+6 downto 7*i),
--                 txOutClk        => dsTxOutClk(7*i+6 downto 7*i),
            txClk     => open,
            txClkIn   => recTimingClk,
            config    => xpmConfig.dsLink(7*i+6 downto 7*i),
            status    => dsLinkStatus (7*i+6 downto 7*i));
   end generate;

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

end top_level;

