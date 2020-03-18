-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmGthUltrascaleTWrapperSim.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2020-03-16
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
      --  Transmit clocking
      devClkOut        : out sl;
      --  Receive clocking
      timRefClkP       : in  sl;
      timRefClkN       : in  sl;
      --
      stableClk        : in  sl;
      txData           : in  slv(15 downto 0);
      txDataK          : in  slv( 1 downto 0);
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

   signal icuRefClk      : sl;
   signal cuRefClk       : sl;
   signal cuRefClkGt     : sl;
   signal cuTxClk        : sl;
   signal cuRxClk        : sl;
   signal cuTx           : TimingPhyType := TIMING_PHY_INIT_C;
   signal tpgRst         : sl;
   signal cuTxStatus     : TimingPhyStatusType;
   signal cuRxControl    : TimingPhyControlType := TIMING_PHY_CONTROL_INIT_C;
   signal cuRxStatus     : TimingPhyStatusType  := TIMING_PHY_STATUS_INIT_C;

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

  devClkOut            <= cuRefClk;
    
  U_TPG : entity lcls_timing_core.TPGMiniStream
    generic map ( NUM_EDEFS => 1,
                  AC_PERIOD => 331534 )   -- 360Hz syncd to 71.4kHz
    port map ( config     => TPG_CONFIG_INIT_C,
               edefConfig => TPG_MINI_EDEF_CONFIG_INIT_C,
               txClk      => cuTxClk,
               txRst      => tpgRst,
               txRdy      => '1',
               txData     => cuTx.data,
               txDataK    => cuTx.dataK );

  TIMREFCLK_IBUFDS_GTE3 : IBUFDS_GTE3
    generic map (
      REFCLK_EN_TX_PATH  => '0',
      REFCLK_HROW_CK_SEL => "01",    -- 2'b01: ODIV2 = Divide-by-2 version of O
      REFCLK_ICNTL_RX    => "00")
    port map (
      I     => timRefClkP,
      IB    => timRefClkN,
      CEB   => '0',
      ODIV2 => icuRefClk,   -- 119 MHz
      O     => cuRefClkGt); -- 238 MHz

  U_BUFG_GT : BUFG_GT
    port map ( O       => cuRefClk,
               CE      => '1',
               CEMASK  => '1',
               CLR     => '0',
               CLRMASK => '1',
               DIV     => "000",           -- Divide-by-1
               I       => icuRefClk );

  U_BpTx : entity lcls_timing_core.TimingGtCoreWrapper
    generic map ( ADDR_BITS_G       => 14,
                  AXIL_BASE_ADDR_G  => AXIL_BASE_ADDR_G,
                  GTH_DRP_OFFSET_G  => x"00004000")
    port map ( -- AXI-Lite Port
      axilClk         => regClk,
      axilRst         => regRst,
      axilReadMaster  => axilReadMaster,
      axilReadSlave   => axilReadSlave ,
      axilWriteMaster => axilWriteMaster,
      axilWriteSlave  => axilWriteSlave ,

      stableClk       => regClk,
      stableRst       => regRst,
      gtRefClk        => cuRefClkGt,
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
