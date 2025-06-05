-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmGthUltrascaleTWrapperSim.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2025-06-04
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Interface to sensor link MGT
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


library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;
use lcls_timing_core.TPGPkg.all;
use lcls_timing_core.TPGMiniEdefPkg.all;

library l2si_core;
use l2si_core.XpmPkg.all;

library unisim;
use unisim.vcomponents.all;


entity XpmGthUltrascaleTWrapperSim is
   generic ( GTGCLKRX   : boolean := true;
             AXIL_BASE_ADDR_G : slv(31 downto 0) );
   port (
      gtTxP            : out sl;
      gtTxN            : out sl;
      gtRxP            : in  sl;
      gtRxN            : in  sl;
      timRefClkGt      : in  sl;
      --
      stableClk        : in  sl;
      rxData           : out TimingRxType;
      rxClk            : out sl;
      rxRst            : out sl;
      txClk            : out sl;
      txClkIn          : in  sl;
      config           : in  XpmLinkConfigType;
      status           : out XpmLinkStatusType;
      --
      regClk           : in  sl;
      regRst           : in  sl;
      axilReadMaster   : in  AxiLiteReadMasterType;
      axilReadSlave    : out AxiLiteReadSlaveType;
      axilWriteMaster  : in  AxiLiteWriteMasterType;
      axilWriteSlave   : out AxiLiteWriteSlaveType
      );
end XpmGthUltrascaleTWrapperSim;

architecture rtl of XpmGthUltrascaleTWrapperSim is

   signal cuTxClk        : sl;
   signal cuRxClk        : sl;
   signal cuTx           : TimingPhyType := TIMING_PHY_INIT_C;
   signal tpgRst         : sl;
   signal cuTxStatus     : TimingPhyStatusType;
   signal cuRxControl    : TimingPhyControlType := TIMING_PHY_CONTROL_INIT_C;
   signal cuRxStatus     : TimingPhyStatusType  := TIMING_PHY_STATUS_INIT_C;

   constant GT_INDEX_C   : integer := 0;
   constant MINI_INDEX_C : integer := 1;
   signal axilReadMasters  : AxiLiteReadMasterArray (1 downto 0);
   signal axilReadSlaves   : AxiLiteReadSlaveArray  (1 downto 0);
   signal axilWriteMasters : AxiLiteWriteMasterArray(1 downto 0);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray (1 downto 0);
   constant AXIL_XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(1 downto 0) := genAxiLiteConfig(2, AXIL_BASE_ADDR_G, 24, 20);
   
begin

  txClk                <= cuTxClk;
  rxClk                <= cuRxClk;
  cuTx.control.reset   <= config.txReset;
  cuRxControl.reset    <= config.rxreset;
  cuRxControl.pllReset <= config.rxPllReset;
  status.rxReady       <= cuRxStatus.bufferByDone;
  status.txReady       <= cuTxStatus.resetDone;

  rxRst                <= not cuRxStatus.bufferByDone;
  tpgRst               <= not cuTxStatus.resetDone;

   --------------------------
   -- AXI-Lite: Crossbar Core
   --------------------------  
   U_XBAR : entity surf.AxiLiteCrossbar
      generic map (
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => AXIL_XBAR_CONFIG_C'length,
         MASTERS_CONFIG_G   => AXIL_XBAR_CONFIG_C)
      port map (
         axiClk              => regClk,
         axiClkRst           => regRst,
         sAxiWriteMasters(0) => axilWriteMaster,
         sAxiWriteSlaves(0)  => axilWriteSlave,
         sAxiReadMasters(0)  => axilReadMaster,
         sAxiReadSlaves(0)   => axilReadSlave,
         mAxiWriteMasters    => axilWriteMasters,
         mAxiWriteSlaves     => axilWriteSlaves,
         mAxiReadMasters     => axilReadMasters,
         mAxiReadSlaves      => axilReadSlaves);

  U_TPG : entity lcls_timing_core.TPGMiniCore
    port map ( txClk          => cuTxClk,
               txRst          => tpgRst,
               txRdy          => '1',
               txData(0)      => cuTx.data,
               txData(1)      => open,
               txDataK(0)     => cuTx.dataK,
               txDataK(1)     => open,
               axiClk         => regClk,
               axiRst         => regRst,
               axiReadMaster  => axilReadMasters (MINI_INDEX_C),
               axiReadSlave   => axilReadSlaves  (MINI_INDEX_C),
               axiWriteMaster => axilWriteMasters(MINI_INDEX_C),
               axiWriteSlave  => axilWriteSlaves (MINI_INDEX_C) );
               
  U_BpTx : entity lcls_timing_core.TimingGtCoreWrapper
    generic map ( ADDR_BITS_G       => 14,
                  AXIL_BASE_ADDR_G  => AXIL_BASE_ADDR_G,
                  GTH_DRP_OFFSET_G  => x"00004000")
    port map ( -- AXI-Lite Port
      axilClk         => regClk,
      axilRst         => regRst,
      axilReadMaster  => axilReadMasters (GT_INDEX_C),
      axilReadSlave   => axilReadSlaves  (GT_INDEX_C),
      axilWriteMaster => axilWriteMasters(GT_INDEX_C),
      axilWriteSlave  => axilWriteSlaves (GT_INDEX_C),

      stableClk       => regClk,
      stableRst       => regRst,
      gtRefClk        => timRefClkGt,
      gtRefClkDiv2    => '0',
      gtRxP           => gtRxP,
      gtRxN           => gtRxN,
      gtTxP           => gtTxP,
      gtTxN           => gtTxN,
      -- Rx ports
      rxControl       => cuRxControl,
      rxStatus        => cuRxStatus,
      rxUsrClkActive  => '1',
      rxCdrStable     => open,
      rxUsrClk        => cuRxClk,
      rxData          => rxData.data,
      rxDataK         => rxData.dataK,
      rxDispErr       => rxData.dspErr,
      rxDecErr        => rxData.decErr,
      rxOutClk        => cuRxClk,
      -- Tx Ports
      txControl       => cuTx.control,
      txStatus        => cuTxStatus,
      txUsrClk        => cuTxClk,
      txUsrClkActive  => '1',     -- activate for now
      txData          => cuTx.data,
      txDataK         => cuTx.dataK,
      txOutClk        => cuTxClk,
      loopback        => "000" );
    
end rtl;
