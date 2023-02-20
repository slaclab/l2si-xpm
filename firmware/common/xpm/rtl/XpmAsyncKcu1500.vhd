-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmAsyncKcu1500.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2023-02-17
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--
-- This module will generate L2S-I timing (XPM) for use in teststands.
--
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
use surf.AxiLitePkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;
use lcls_timing_core.TPGPkg.all;
use lcls_timing_core.TPGMiniEdefPkg.all;

library l2si_core;
use l2si_core.CuTimingPkg.all;
use l2si_core.XpmPkg.all;
use l2si_core.XpmExtensionPkg.all;

library l2si;

entity XpmAsyncKcu1500 is
   generic (
     TPD_G               : time    := 1 ns;
     AXIL_BASE_G         : slv(31 downto 0) );
   port (
     signal axilClk               : in  sl;
     signal axilRst               : in  sl;
     signal axilReadMaster        : in  AxiLiteReadMasterType;
     signal axilReadSlave         : out AxiLiteReadSlaveType;
     signal axilWriteMaster       : in  AxiLiteWriteMasterType;
     signal axilWriteSlave        : out AxiLiteWriteSlaveType;

     -- signal usRefClkGt            : in  sl;
     -- signal usRxP                 : in  sl;
     -- signal usRxN                 : in  sl;
     -- signal usTxP                 : out sl;
     -- signal usTxN                 : out sl;
     signal usRecClk              : in  sl;
     -- signal usRefClk              : out sl;
     signal usRx                  : in  TimingRxType;
     signal usRxStatus            : in  TimingPhyStatusType;
     
     signal timingPhyClk          : in  sl;
     signal timingPhyRst          : in  sl;
     -- signal timingPhy             : in  TimingPhyType;
     signal recStream             : out XpmStreamType );
end XpmAsyncKcu1500;

architecture rtl of XpmAsyncKcu1500 is

   constant GTH_INDEX_C  : integer := 0;
   constant TIM_INDEX_C  : integer := 1;
   constant AXI_XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(1 downto 0) := genAxiLiteConfig(2, AXIL_BASE_G, 19, 18);

   signal axilReadMasters  : AxiLiteReadMasterArray (AXI_XBAR_CONFIG_C'range);
   signal axilReadSlaves   : AxiLiteReadSlaveArray  (AXI_XBAR_CONFIG_C'range) := (others=>AXI_LITE_READ_SLAVE_EMPTY_OK_C);
   signal axilWriteMasters : AxiLiteWriteMasterArray(AXI_XBAR_CONFIG_C'range);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray (AXI_XBAR_CONFIG_C'range) := (others=>AXI_LITE_WRITE_SLAVE_EMPTY_OK_C);

   -- Timing Interface (timingClk domain)
   signal txStreams   : TimingSerialArray(2 downto 0);
   signal txStreamIds : Slv4Array        (2 downto 0);
   signal txAdvance   : slv              (2 downto 0) := (others=>'0');
--   signal usRx        : TimingRxType;
   signal usRxControl : TimingPhyControlType := TIMING_PHY_CONTROL_INIT_C;
--   signal usRxStatus  : TimingPhyStatusType;
   signal usRxClk     : sl;
   signal usTxOutClk  : sl;
   signal usRxTimingBus : TimingBusType;
   signal usRxMessage : TimingMessageType;
   signal usRxVector  : slv(TIMING_MESSAGE_BITS_C-1 downto 0);
   signal usRxStrobe  : sl;
   signal usRxValid   : sl;
   signal usRxExtn    : TimingExtensionArray;

   signal xpmVector   : slv(XPM_MESSAGE_BITS_C-1 downto 0);
   signal xpmValid    : sl;
   signal cuRecVector : slv(CU_TIMING_BITS_C-1 downto 0);
   signal cuRecValid  : sl;
   
begin

--  usRecClk <= usRxClk;
--  usRefClk <= usTxOutClk;
  
  U_XBAR : entity surf.AxiLiteCrossbar
    generic map (
      TPD_G              => TPD_G,
      DEC_ERROR_RESP_G   => AXI_RESP_DECERR_C,
      NUM_SLAVE_SLOTS_G  => 1,
      NUM_MASTER_SLOTS_G => AXI_XBAR_CONFIG_C'length,
      MASTERS_CONFIG_G   => AXI_XBAR_CONFIG_C)
    port map (
      axiClk              => axilClk,
      axiClkRst           => axilRst,
      sAxiWriteMasters(0) => axilWriteMaster,
      sAxiWriteSlaves(0)  => axilWriteSlave,
      sAxiReadMasters(0)  => axilReadMaster,
      sAxiReadSlaves (0)  => axilReadSlave,
      mAxiWriteMasters    => axilWriteMasters,
      mAxiWriteSlaves     => axilWriteSlaves,
      mAxiReadMasters     => axilReadMasters,
      mAxiReadSlaves      => axilReadSlaves);

  -- TimingGtCoreWrapper_1 : entity lcls_timing_core.TimingGtCoreWrapper
  --   generic map (EXTREF_G         => true,
  --                ADDR_BITS_G      => 14,
  --                AXIL_BASE_ADDR_G => AXI_XBAR_CONFIG_C(GTH_INDEX_C).baseAddr,
  --                GTH_DRP_OFFSET_G => x"00004000")
  --   port map (
  --     axilClk         => axilClk,
  --     axilRst         => axilRst,
  --     axilReadMaster  => axilReadMasters (GTH_INDEX_C),
  --     axilReadSlave   => axilReadSlaves  (GTH_INDEX_C),
  --     axilWriteMaster => axilWriteMasters(GTH_INDEX_C),
  --     axilWriteSlave  => axilWriteSlaves (GTH_INDEX_C),
  --     stableClk       => axilClk,
  --     stableRst       => axilRst,
  --     gtRefClk        => usRefClkGt,
  --     gtRefClkDiv2    => '0',
  --     gtRxP           => usRxP,
  --     gtRxN           => usRxN,
  --     gtTxP           => usTxP,
  --     gtTxN           => usTxN,
  --     rxControl       => usRxControl,
  --     rxStatus        => usRxStatus,
  --     rxUsrClkActive  => '1',
  --     rxCdrStable     => open,
  --     rxUsrClk        => usRxClk,
  --     rxData          => usRx.data,
  --     rxDataK         => usRx.dataK,
  --     rxDispErr       => usRx.dspErr,
  --     rxDecErr        => usRx.decErr,
  --     rxOutClk        => usRxClk,
  --     txControl       => usRxControl,
  --     txStatus        => open,
  --     txUsrClk        => timingPhyClk,
  --     txUsrClkActive  => '1',
  --     txData          => timingPhy.data,
  --     txDataK         => timingPhy.dataK,
  --     txOutClk        => usTxOutClk,
  --     loopback        => "000");

  U_UsRx : entity lcls_timing_core.TimingCore
    generic map (
      AXIL_BASE_ADDR_G => AXI_XBAR_CONFIG_C(TIM_INDEX_C).baseAddr,
      CLKSEL_MODE_G    => "LCLSII",
      AXIL_RINGB_G     => false,
      USE_TPGMINI_G    => false )
    port map (
      gtTxUsrClk          => timingPhyClk,
      gtTxUsrRst          => timingPhyRst,
      
--      gtRxRecClk          => usRxClk,
      gtRxRecClk          => usRecClk,
      gtRxData            => usRx.data,
      gtRxDataK           => usRx.dataK,
      gtRxDispErr         => usRx.dspErr,
      gtRxDecErr          => usRx.decErr,
      gtRxControl         => usRxControl,
      gtRxStatus          => usRxStatus,
      appTimingClk        => timingPhyClk,
      appTimingRst        => timingPhyRst,
      appTimingBus        => usRxTimingBus,

      axilClk             => axilClk,
      axilRst             => axilRst,
      axilReadMaster      => axilReadMasters (TIM_INDEX_C),
      axilReadSlave       => axilReadSlaves  (TIM_INDEX_C),
      axilWriteMaster     => axilWriteMasters(TIM_INDEX_C),
      axilWriteSlave      => axilWriteSlaves (TIM_INDEX_C));

  --
  --  Reconstructed streams
  --
  usRxVector <= toSlv(usRxTimingBus.message);
  usRxValid  <= usRxTimingBus.valid;
  usRxStrobe <= usRxTimingBus.strobe;
  
  U_UsRecSerializer : entity lcls_timing_core.WordSerializer
    generic map (
      NWORDS_G => TIMING_MESSAGE_WORDS_C)
    port map (
      txClk    => timingPhyClk,
      txRst    => timingPhyRst,
      fiducial => usRxStrobe,
      words    => usRxVector,
      ready    => usRxValid,
      advance  => txAdvance (0),
      stream   => txStreams (0));

  cuRecVector <= usRxExtn(CU_TIMING_STREAM_ID_C).data(TIMING_EXTENSION_MESSAGE_BITS_C-1 downto
                                                      TIMING_EXTENSION_MESSAGE_BITS_C-CU_TIMING_BITS_C);
  cuRecValid  <= usRxExtn(CU_TIMING_STREAM_ID_C).valid;

  U_CuRecSerializer : entity lcls_timing_core.WordSerializer
    generic map (
      NWORDS_G => CU_TIMING_WORDS_C)
    port map (
      txClk    => timingPhyClk,
      txRst    => timingPhyRst,
      fiducial => usRxStrobe,
      words    => cuRecVector,
      ready    => cuRecValid,
      advance  => txAdvance (1),
      stream   => txStreams (1));
  xpmVector <= usRxExtn(XPM_STREAM_ID_C).data(TIMING_EXTENSION_MESSAGE_BITS_C-1 downto
                                              TIMING_EXTENSION_MESSAGE_BITS_C-XPM_MESSAGE_BITS_C);
  xpmValid  <= usRxExtn(XPM_STREAM_ID_C).valid;

  U_XpmRecSerializer : entity lcls_timing_core.WordSerializer
    generic map (
      NWORDS_G => XPM_MESSAGE_WORDS_C)
    port map (
      txClk    => timingPhyClk,
      txRst    => timingPhyRst,
      fiducial => usRxStrobe,
      words    => xpmVector,
      ready    => xpmValid,
      advance  => txAdvance (2),
      stream   => txStreams (2));

  txStreamIds(0) <= toSlv(0, 4);
  txStreamIds(1) <= toSlv(CU_TIMING_STREAM_ID_C, 4);
  txStreamIds(2) <= toSlv(XPM_STREAM_ID_C, 4);

  U_SimSerializer : entity lcls_timing_core.TimingSerializer
    generic map (
      STREAMS_C => 3)
    port map (
      clk       => timingPhyClk,
      rst       => timingPhyRst,
      fiducial  => usRxStrobe,
      streams   => txStreams,
      streamIds => txStreamIds,
      advance   => txAdvance,
      data      => open,
      dataK     => open);

  recStream.fiducial <= usRxStrobe;
  recStream.streams  <= txStreams;
  recStream.advance  <= txAdvance;

end rtl;

