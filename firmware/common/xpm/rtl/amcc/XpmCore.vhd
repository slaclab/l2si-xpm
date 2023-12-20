-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmCore.vhd
-- Author     : Matt Weaver (weaver@slac.stanford.edu)
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-08
-- Last update: 2023-12-20
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
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


library surf;
use surf.StdRtlPkg.all;
use surf.AxiStreamPkg.all;
use surf.SsiPkg.all;
use surf.AxiLitePkg.all;
use surf.AxiPkg.all;
use surf.I2cPkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;

library l2si_core;
use l2si_core.XpmPkg.all;

library amc_carrier_core;
use amc_carrier_core.AmcCarrierSysRegPkg.all;
use amc_carrier_core.AmcCarrierPkg.all;

library unisim;
use unisim.vcomponents.all;

library l2si;

entity XpmCore is
   generic (
      TPD_G               : time                := 1 ns;   -- Simulation only parameter
      SIM_SPEEDUP_G       : boolean             := false;  -- Simulation only parameter
      MPS_SLOT_G          : boolean             := false;  -- false = Normal Operation, true = MPS message concentrator (Slot#2 only)
      FSBL_G              : boolean             := false;  -- false = Normal Operation, true = First Stage Boot loader
      BUILD_INFO_G        : BuildInfoType;
      APP_TYPE_G          : AppType             := APP_NULL_TYPE_C;
      OVERRIDE_BSI_G      : boolean             := false;  -- false = Normal Operation, true = use IP_ADDR, MAC_ADDR generics
      IP_ADDR_G           : slv(31 downto 0)    := x"0A02A8C0";
      MAC_ADDR_G          : slv(47 downto 0)    := x"010300564400";
      FFB_CLIENT_SIZE_G   : positive            := 1;
      DIAGNOSTIC_SIZE_G   : positive            := 1;
      DIAGNOSTIC_CONFIG_G : AxiStreamConfigType := ssiAxiStreamConfig(4);
      USE_XTPG_G          : boolean             := false;
      US_RX_ENABLE_INIT_G : boolean             := true;
      CU_RX_ENABLE_INIT_G : boolean             := false;
      CU_ASYNC_G          : boolean             := false;
      L2_FROM_CU_G        : boolean             := false;
      UED_MODE_G          : boolean             := false );
   port (
      ----------------------
      -- Top Level Interface
      ----------------------
      -- AXI-Lite Interface (regClk domain)
      -- Address Range = [0x80000000:0xFFFFFFFF]
      regClk           : out   sl;
      regRst           : out   sl;
      regReadMaster    : out   AxiLiteReadMasterType;
      regReadSlave     : in    AxiLiteReadSlaveType;
      regWriteMaster   : out   AxiLiteWriteMasterType;
      regWriteSlave    : in    AxiLiteWriteSlaveType;
      -- Streaming input (regClk domain)
      ibDebugMaster    : out   AxiStreamMasterType;
      ibDebugSlave     : in    AxiStreamSlaveType := AXI_STREAM_SLAVE_FORCE_C;
      obDebugMaster    : in    AxiStreamMasterType;
      obDebugSlave     : out   AxiStreamSlaveType;
      -- Timing Interface (timingClk domain)
--      timingData        : out   TimingRxType;
      recStream        : out   XpmStreamType;
      timingPhy        : in    TimingPhyType      := TIMING_PHY_INIT_C;  -- Input for timing generator only
      timingPhyClk     : out   sl;
      timingPhyRst     : out   sl;
      -- BSI Interface (bsiClk domain) 
      bsiClk           : in    sl                 := '0';
      bsiRst           : in    sl                 := '0';
      bsiBus           : out   BsiBusType;
      -- Reference Clocks and Resets
      recTimingClk     : out   sl;
      recTimingRst     : out   sl;
      recTimingLkN     : out   sl;
      ref125MHzClk     : out   sl;
      ref125MHzRst     : out   sl;
      ref156MHzClk     : out   sl;
      ref156MHzRst     : out   sl;
      ref312MHzClk     : out   sl;
      ref312MHzRst     : out   sl;
      ref625MHzClk     : out   sl;
      ref625MHzRst     : out   sl;
      gthFabClk        : out   sl;
      ----------------
      -- Core Ports --
      ----------------
      -- Common Fabricate Clock
      fabClkP          : in    sl;
      fabClkN          : in    sl;
      -- Backplane Ethernet Ports
      ethRxP           : in    slv(3 downto 0);
      ethRxN           : in    slv(3 downto 0);
      ethTxP           : out   slv(3 downto 0);
      ethTxN           : out   slv(3 downto 0);
      ethClkP          : in    sl;
      ethClkN          : in    sl;
      ipAddr           : out   slv(31 downto 0);
      -- LCLS-I Timing Ports
      cuRxEnable       : in    sl                 := '0';
      cuRecClk         : out   sl;
      cuRecFiducial    : out   sl;
      bpTxData         : in    slv(15 downto 0);
      bpTxDataK        : in    slv(1 downto 0);
      cuSync           : out   sl;
      -- Upstream Timing Ports
      usRxEnable       : in    sl                 := '1';
      usRxP            : in    sl;
      usRxN            : in    sl;
      usTxP            : out   sl;
      usTxN            : out   sl;
      usRefClk         : in    sl;
      usRefClkGt       : in    sl;
      --
      timingRecClkOutP : out   sl;
      timingRecClkOutN : out   sl;
      timingClkScl     : inout sl;
      timingClkSda     : inout sl;
      -- Timing Reference
      timingClkSel     : out   sl;
      timingRefClkGt   : in    sl;
      timingRefClk     : in    sl;
      timingRxP        : in    sl;
      timingRxN        : in    sl;
      timingTxP        : out   sl;
      timingTxN        : out   sl;
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
      --
      hsrScl           : inout Slv3Array(1 downto 0);
      hsrSda           : inout Slv3Array(1 downto 0);
      amcScl           : inout slv (1 downto 0);
      amcSda           : inout slv (1 downto 0);
      -- DDR3L SO-DIMM Ports
      ddrScl           : inout sl;
      ddrSda           : inout sl;
      -- SYSMON Ports
      vPIn             : in    sl;
      vNIn             : in    sl);
end XpmCore;

architecture mapping of XpmCore is

   constant AXI_ERROR_RESP_C : slv(1 downto 0) := AXI_RESP_DECERR_C;

   signal axilClk          : sl;
   signal axilRst          : sl;
   signal axilReadMasters  : AxiLiteReadMasterArray (1 downto 0);
   signal axilReadSlaves   : AxiLiteReadSlaveArray (1 downto 0);
   signal axilWriteMasters : AxiLiteWriteMasterArray(1 downto 0);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray (1 downto 0);

   signal axiClk         : sl;
   signal axiRst         : sl;
   signal axiWriteMaster : AxiWriteMasterType;
   signal axiWriteSlave  : AxiWriteSlaveType;
   signal axiReadMaster  : AxiReadMasterType;
   signal axiReadSlave   : AxiReadSlaveType;

   signal timingReadMaster  : AxiLiteReadMasterType;
   signal timingReadSlave   : AxiLiteReadSlaveType;
   signal timingWriteMaster : AxiLiteWriteMasterType;
   signal timingWriteSlave  : AxiLiteWriteSlaveType;

   signal ethReadMaster  : AxiLiteReadMasterType;
   signal ethReadSlave   : AxiLiteReadSlaveType;
   signal ethWriteMaster : AxiLiteWriteMasterType;
   signal ethWriteSlave  : AxiLiteWriteSlaveType;

   signal cuGtReadMaster  : AxiLiteReadMasterType;
   signal cuGtReadSlave   : AxiLiteReadSlaveType;
   signal cuGtWriteMaster : AxiLiteWriteMasterType;
   signal cuGtWriteSlave  : AxiLiteWriteSlaveType;

   signal usGtReadMaster  : AxiLiteReadMasterType;
   signal usGtReadSlave   : AxiLiteReadSlaveType;
   signal usGtWriteMaster : AxiLiteWriteMasterType;
   signal usGtWriteSlave  : AxiLiteWriteSlaveType;

   signal amcReadMasters  : AxiLiteReadMasterArray (2 downto 0);
   signal amcReadSlaves   : AxiLiteReadSlaveArray (2 downto 0);
   signal amcWriteMasters : AxiLiteWriteMasterArray(2 downto 0);
   signal amcWriteSlaves  : AxiLiteWriteSlaveArray (2 downto 0);

   signal amcReadMaster  : AxiLiteReadMasterType;
   signal amcReadSlave   : AxiLiteReadSlaveType;
   signal amcWriteMaster : AxiLiteWriteMasterType;
   signal amcWriteSlave  : AxiLiteWriteSlaveType;

   signal bsiMac    : slv(47 downto 0);
   signal bsiIp     : slv(31 downto 0);
   signal ethPhyRdy : sl;

   signal localMac   : slv(47 downto 0);
   signal localIp    : slv(31 downto 0);
   signal localAppId : slv(15 downto 0);

   signal iusRefClk                       : sl;
   signal usRecClk, usRecClkRst           : sl;
   signal usTxOutClk                      : sl;

   signal usRxControl            : TimingPhyControlType;  --reset,inhibit,polarity,bufferByRst,pllReset
   signal usRxStatus, usTxStatus : TimingPhyStatusType;  --locked,resetDone,bufferByDone,bufferByErr
   signal usRx                   : TimingRxType;
   signal usRxStable             : sl;

   signal irecTimingClk : sl;

   signal cuRx        : TimingRxType;
   signal cuRefClk    : sl;
   signal cuRxClk     : sl;
   signal cuRxRst     : sl;
   signal cuRxStatus  : TimingPhyStatusType := TIMING_PHY_STATUS_INIT_C;
   signal cuRxControl : TimingPhyControlType;
   signal xbarControl : XpmLinkConfigType := XPM_LINK_CONFIG_INIT_C;
   signal xbarStatus  : XpmLinkStatusType;

   constant AMC_XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(2 downto 0) := genAxiLiteConfig(3, BSA_ADDR_C, 24, 20);

   constant SFP_DEVICE_MAP_C : I2cAxiLiteDevArray(3 downto 0) := (
      -- PCA9506
      0 => MakeI2cAxiLiteDevType("0100000", 8, 8, '0'),
      -- PCA9547 I2C Mux
      1 => MakeI2cAxiLiteDevType("1110000", 8, 0, '0'),
      -- SFP A0
      2 => MakeI2cAxiLiteDevType("1010000", 8, 8, '0'),
      -- SFP A2
      3 => MakeI2cAxiLiteDevType("1010001", 8, 8, '0'));

begin

   timingPhyClk <= usTxOutClk;
   timingPhyRst <= not usTxStatus.resetDone;
   ipAddr       <= localIp;
   recTimingClk <= irecTimingClk;
   cuRecClk     <= cuRxClk;
   
   GEN_BSI_OVERRIDE : if OVERRIDE_BSI_G = true generate
      localIp  <= IP_ADDR_G;
      localMac <= MAC_ADDR_G;
   end generate GEN_BSI_OVERRIDE;

   GEN_NO_BSI_OVERRIDE : if OVERRIDE_BSI_G = false generate
      localIp  <= bsiIp;
      localMac <= bsiMac;
   end generate GEN_NO_BSI_OVERRIDE;

   regClk <= axilClk;
   regRst <= axilRst;

   -- Send a copy of the timing clock to the AMC's clock cleaner
   ClkOutBufDiff_Inst : entity surf.ClkOutBufDiff
      generic map (
         TPD_G        => TPD_G,
         XIL_DEVICE_G => "ULTRASCALE")
      port map (
         clkIn   => irecTimingClk,
         clkOutP => timingRecClkOutP,
         clkOutN => timingRecClkOutN);

   --------------------------------
   -- Common Clock and Reset Module
   -------------------------------- 
   U_ClkAndRst : entity l2si.XpmClkAndRst
      generic map (
         TPD_G         => TPD_G,
         SIM_SPEEDUP_G => SIM_SPEEDUP_G)
      port map (
         -- Reference Clocks and Resets
         ref125MHzClk => ref125MHzClk,
         ref125MHzRst => ref125MHzRst,
         ref156MHzClk => ref156MHzClk,
         ref156MHzRst => ref156MHzRst,
         ref312MHzClk => ref312MHzClk,
         ref312MHzRst => ref312MHzRst,
         ref625MHzClk => ref625MHzClk,
         ref625MHzRst => ref625MHzRst,
         gthFabClk    => gthFabClk,
         -- AXI-Lite Clocks and Resets
         axilClk      => axilClk,
         axilRst      => axilRst,
         ----------------
         -- Core Ports --
         ----------------   
         -- Common Fabricate Clock
         fabClkP      => fabClkP,
         fabClkN      => fabClkN);

   ------------------------------------
   -- Ethernet Module (ATCA ZONE 2)
   ------------------------------------
   U_Eth : entity amc_carrier_core.AmcCarrierEth
      generic map (
         TPD_G => TPD_G)
      port map (
         -- Local Configuration
         localMac             => localMac,
         localIp              => localIp,
         ethPhyReady          => ethPhyRdy,
         -- Master AXI-Lite Interface
         mAxilReadMasters     => axilReadMasters,
         mAxilReadSlaves      => axilReadSlaves,
         mAxilWriteMasters    => axilWriteMasters,
         mAxilWriteSlaves     => axilWriteSlaves,
         -- AXI-Lite Interface
         axilClk              => axilClk,
         axilRst              => axilRst,
         axilReadMaster       => ethReadMaster,
         axilReadSlave        => ethReadSlave,
         axilWriteMaster      => ethWriteMaster,
         axilWriteSlave       => ethWriteSlave,
         -- 0 => Stat updates (port 8197)
         -- 1 => BLD/BSSS
         obTimingEthMsgMasters(0) => obDebugMaster,
         obTimingEthMsgMasters(1) => AXI_STREAM_MASTER_INIT_C,
         obTimingEthMsgSlaves (0) => obDebugSlave,
         obTimingEthMsgSlaves (1) => open,
         ibTimingEthMsgSlaves     => (others=>AXI_STREAM_SLAVE_FORCE_C),
         -- BSA Ethernet Interface
         obBsaMasters         => (others => AXI_STREAM_MASTER_INIT_C),
         ibBsaSlaves          => (others => AXI_STREAM_SLAVE_FORCE_C),
         -- Application Debug Interface
         obAppDebugMaster     => AXI_STREAM_MASTER_INIT_C,
         ibAppDebugMaster     => ibDebugMaster,
         ibAppDebugSlave      => ibDebugSlave,
         ----------------------
         -- Top Level Interface
         ----------------------
         obBpMsgClientMaster  => AXI_STREAM_MASTER_INIT_C,
         ibBpMsgClientSlave   => AXI_STREAM_SLAVE_FORCE_C,
         obBpMsgServerMaster  => AXI_STREAM_MASTER_INIT_C,
         ibBpMsgServerSlave   => AXI_STREAM_SLAVE_FORCE_C,
         ----------------
         -- Core Ports --
         ----------------   
         -- ETH Ports
         ethRxP               => ethRxP,
         ethRxN               => ethRxN,
         ethTxP               => ethTxP,
         ethTxN               => ethTxN,
         ethClkP              => ethClkP,
         ethClkN              => ethClkN);

   ----------------------------------   
   -- Register Address Mapping Module
   ----------------------------------   
   U_SysReg : entity amc_carrier_core.AmcCarrierSysReg
      generic map (
         TPD_G        => TPD_G,
         BUILD_INFO_G => BUILD_INFO_G,
         APP_TYPE_G   => APP_TYPE_G,
         FSBL_G       => FSBL_G)
      port map (
         -- Primary AXI-Lite Interface
         axilClk           => axilClk,
         axilRst           => axilRst,
         sAxilReadMasters  => axilReadMasters,
         sAxilReadSlaves   => axilReadSlaves,
         sAxilWriteMasters => axilWriteMasters,
         sAxilWriteSlaves  => axilWriteSlaves,
         -- Timing AXI-Lite Interface
         timingReadMaster  => timingReadMaster,
         timingReadSlave   => timingReadSlave,
         timingWriteMaster => timingWriteMaster,
         timingWriteSlave  => timingWriteSlave,
         -- BSA AXI-Lite Interface
         bsaReadMaster     => amcReadMaster,
         bsaReadSlave      => amcReadSlave,
         bsaWriteMaster    => amcWriteMaster,
         bsaWriteSlave     => amcWriteSlave,
         -- ETH PHY AXI-Lite Interface
         ethReadMaster     => ethReadMaster,
         ethReadSlave      => ethReadSlave,
         ethWriteMaster    => ethWriteMaster,
         ethWriteSlave     => ethWriteSlave,
         -- DDR PHY AXI-Lite Interface
         ddrReadMaster     => usGtReadMaster,
         ddrReadSlave      => usGtReadSlave,
         ddrWriteMaster    => usGtWriteMaster,
         ddrWriteSlave     => usGtWriteSlave,
         ddrMemReady       => '1',
         ddrMemError       => '0',
         -- MPS PHY AXI-Lite Interface
         mpsReadMaster     => cuGtReadMaster,
         mpsReadSlave      => cuGtReadSlave,
         mpsWriteMaster    => cuGtWriteMaster,
         mpsWriteSlave     => cuGtWriteSlave,
         -- Local Configuration
         localMac          => bsiMac,
         localIp           => bsiIp,
         ethLinkUp         => ethPhyRdy,
         ----------------------
         -- Top Level Interface
         ----------------------              
         -- Application AXI-Lite Interface
         appReadMaster     => regReadMaster,
         appReadSlave      => regReadSlave,
         appWriteMaster    => regWriteMaster,
         appWriteSlave     => regWriteSlave,
         -- BSI Interface
         bsiBus            => bsiBus,
         ----------------
         -- Core Ports --
         ----------------   
         -- Crossbar Ports
         xBarSin           => xBarSin,
         xBarSout          => xBarSout,
         xBarConfig        => xBarConfig,
         xBarLoad          => xBarLoad,
         -- IPMC Ports
         ipmcScl           => ipmcScl,
         ipmcSda           => ipmcSda,
         -- Configuration PROM Ports
         calScl            => calScl,
         calSda            => calSda,
         -- Clock Cleaner Ports
         timingClkScl      => timingClkScl,
         timingClkSda      => timingClkSda,
         -- DDR3L SO-DIMM Ports
         ddrScl            => ddrScl,
         ddrSda            => ddrSda,
         -- SYSMON Ports
         vPIn              => vPIn,
         vNIn              => vNIn);

   --------------
   -- Timing Core
   --------------
   GEN_L2_FROM_CU: if L2_FROM_CU_G generate
     U_Timing : entity l2si.Xpm2TimingFromUsRx
       generic map (
         AXIL_BASE_ADDR_G    => TIMING_ADDR_C )
       port map (
         -- AXI-Lite Interface (axilClk domain)
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilReadMaster  => timingReadMaster,
         axilReadSlave   => timingReadSlave,
         axilWriteMaster => timingWriteMaster,
         axilWriteSlave  => timingWriteSlave,
         usRefClk        => cuRefClk,
         usRefClkRst     => axilRst,
         usRecClk        => cuRxClk,
         usRecClkRst     => cuRxRst,
         usRxEnable      => '1',
         usRx            => cuRx,
         usRxStatus      => cuRxStatus,
         usRxControl     => cuRxControl,
         timingClk       => irecTimingClk,
         timingRst       => recTimingRst,
         timingLkN       => recTimingLkN,
         timingStream    => recStream);

     cuRecFiducial <= '0';
     cuSync        <= '0';
   end generate;
   
   NO_GEN_L2_FROM_CU: if not L2_FROM_CU_G generate
     U_Timing : entity l2si.Xpm2Timing
       generic map (
         AXIL_BASE_ADDR_G    => TIMING_ADDR_C,
         USE_XTPG_G          => USE_XTPG_G,
         US_RX_ENABLE_INIT_G => US_RX_ENABLE_INIT_G,
         CU_RX_ENABLE_INIT_G => CU_RX_ENABLE_INIT_G,
         CU_ASYNC_G          => CU_ASYNC_G )
       port map (
         -- AXI-Lite Interface (axilClk domain)
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilReadMaster  => timingReadMaster,
         axilReadSlave   => timingReadSlave,
         axilWriteMaster => timingWriteMaster,
         axilWriteSlave  => timingWriteSlave,
         usRefClk        => iusRefClk,
         usRefClkRst     => axilRst,
         usRecClk        => usRecClk,
         usRecClkRst     => usRecClkRst,
         usRxEnable      => usRxEnable,
         usRx            => usRx,
         usRxStatus      => usRxStatus,
         usRxControl     => usRxControl,
         cuRefClk        => cuRefClk,
         cuRecClk        => cuRxClk,
         cuRecClkRst     => cuRxRst,
         cuRx            => cuRx,
         cuRxStatus      => cuRxStatus,
         cuRxControl     => cuRxControl,
         cuRxFiducial    => cuRecFiducial,
         cuSync          => cuSync,
         timingClk       => irecTimingClk,
         timingRst       => recTimingRst,
         timingLkN       => recTimingLkN,
         timingStream    => recStream);
   end generate;
   
   -------------------------------------------------------------------------------------------------
   -- Clock Buffers
   -------------------------------------------------------------------------------------------------
   timingClkSel <= '0'       when (CU_RX_ENABLE_INIT_G or UED_MODE_G) else '1';

   GEN_USREFCLK   : if US_RX_ENABLE_INIT_G generate
     iusRefClk <= usRefClk;
   end generate;
   GEN_NOUSREFCLK : if not US_RX_ENABLE_INIT_G generate
     iusRefClk <= timingRefClk;
   end generate;
   
   TimingGtCoreWrapper_1 : entity lcls_timing_core.TimingGtCoreWrapper
      generic map (ADDR_BITS_G      => 14,
                   AXIL_BASE_ADDR_G => DDR_ADDR_C,
                   GTH_DRP_OFFSET_G => x"00004000")
      port map (
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilReadMaster  => usGtReadMaster,
         axilReadSlave   => usGtReadSlave,
         axilWriteMaster => usGtWriteMaster,
         axilWriteSlave  => usGtWriteSlave,
         stableClk       => axilClk,
         stableRst       => axilRst,
         gtRefClk        => usRefClkGt,
         gtRefClkDiv2    => '0',
         gtRxP           => usRxP,
         gtRxN           => usRxN,
         gtTxP           => usTxP,
         gtTxN           => usTxN,
         rxControl       => usRxControl,
         rxStatus        => usRxStatus,
         rxUsrClkActive  => '1',
         rxCdrStable     => usRxStable,
         rxUsrClk        => usRecClk,
         rxData          => usRx.data,
         rxDataK         => usRx.dataK,
         rxDispErr       => usRx.dspErr,
         rxDecErr        => usRx.decErr,
         rxOutClk        => usRecClk,
         txControl       => timingPhy.control,
         txStatus        => usTxStatus,
         txUsrClk        => usTxOutClk,
         txUsrClkActive  => '1',
         txData          => timingPhy.data,
         txDataK         => timingPhy.dataK,
         txOutClk        => usTxOutClk,  -- will this be source synchronous?
         loopback        => "000");

   usRecClkRst <= not usRxStatus.resetDone;

   -- Can't get CPLL Tx/QPLL Rx combination to work, yet
   -- Simplify to standard timing receiver
   -- No Bp Tx broadcast
   --  U_BpTx : entity l2si.XpmGthUltrascaleTWrapper
   U_BpTx : entity l2si.XpmGthUltrascaleTWrapperSim
      generic map (GTGCLKRX         => false,
                   AXIL_BASE_ADDR_G => MPS_ADDR_C)
      port map (stableClk       => axilClk,
                gtTxP           => timingTxP,
                gtTxN           => timingTxN,
                gtRxP           => timingRxP,  -- LCLS-I input
                gtRxN           => timingRxN,
                timRefClkGt     => timingRefClkGt,
                txData          => bpTxData,
                txDataK         => bpTxDataK,
                rxData          => cuRx,
                rxClk           => cuRxClk,
                rxRst           => cuRxRst,
                txClk           => cuRefClk,
                txClkIn         => irecTimingClk,
                config          => xbarControl,
                status          => xbarStatus,
                regClk          => axilClk,
                regRst          => axilRst,
                axilReadMaster  => cuGtReadMaster,
                axilReadSlave   => cuGtReadSlave,
                axilWriteMaster => cuGtWriteMaster,
                axilWriteSlave  => cuGtWriteSlave);

--  xbarControl.txReset    <= xpmConfig.bpLink(0).txReset;
   xbarControl.rxreset    <= cuRxControl .reset;
   xbarControl.rxPllReset <= cuRxControl .pllReset;

   --bpStatus(0).linkUp  <= xbarStatus.txReady;
   --bpStatus(0).ibRecv  <= (others=>'0');
   --bpStatus(0).rxErrs  <= (others=>'0');
   --bpStatus(0).rxLate  <= (others=>'0');

   cuRxStatus.locked       <= xbarStatus.rxReady;
   cuRxStatus.resetDone    <= xbarStatus.rxReady;
   cuRxStatus.bufferByDone <= xbarStatus.rxReady;

   U_AmcCrossBar : entity surf.AxiLiteCrossBar
      generic map (NUM_SLAVE_SLOTS_G  => 1,
                   NUM_MASTER_SLOTS_G => AMC_XBAR_CONFIG_C'length,
                   MASTERS_CONFIG_G   => AMC_XBAR_CONFIG_C)
      port map (
         axiClk              => axilClk,
         axiClkRst           => axilRst,
         sAxiWriteMasters(0) => amcWriteMaster,
         sAxiWriteSlaves(0)  => amcWriteSlave,
         sAxiReadMasters(0)  => amcReadMaster,
         sAxiReadSlaves(0)   => amcReadSlave,
         mAxiWriteMasters    => amcWriteMasters,
         mAxiWriteSlaves     => amcWriteSlaves,
         mAxiReadMasters     => amcReadMasters,
         mAxiReadSlaves      => amcReadSlaves);

   GEN_AMC : for i in 0 to 1 generate
     U_I2C : entity surf.AxiI2cRegMaster
       generic map (
         DEVICE_MAP_G   => SFP_DEVICE_MAP_C,
         AXI_CLK_FREQ_G => 125.0E+6)
       port map (
         scl            => amcScl(i),
         sda            => amcSda(i),
         axiReadMaster  => amcReadMasters (i+1),
         axiReadSlave   => amcReadSlaves (i+1),
         axiWriteMaster => amcWriteMasters(i+1),
         axiWriteSlave  => amcWriteSlaves (i+1),
         axiClk         => axilClk,
         axiRst         => axilRst);
   end generate;

   U_HSRepeater : entity l2si_core.HSRepeater
      generic map (
         AXI_ERROR_RESP_G => AXI_ERROR_RESP_C,
         AXI_BASEADDR_G   => AMC_XBAR_CONFIG_C(0).baseAddr )
      port map (
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilReadMaster  => amcReadMasters(0),
         axilReadSlave   => amcReadSlaves (0),
         axilWriteMaster => amcWriteMasters(0),
         axilWriteSlave  => amcWriteSlaves (0),
         --
         hsrScl          => hsrScl,
         hsrSda          => hsrSda);

end mapping;
