-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : Xpm2TimingFromUsRx.vhd
-- Author     : Matt Weaver <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-08
-- Last update: 2021-01-04
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-- Generate XpmStreams from 119MHz LCLS2 timing on CuRx interface
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
use surf.AxiLitePkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;

library l2si_core;
use l2si_core.CuTimingPkg.all;
use l2si_core.XpmExtensionPkg.all;
use l2si_core.XpmPkg.all;

library unisim;
use unisim.vcomponents.all;

library l2si; 

entity Xpm2TimingFromUsRx is
   generic (
      AXIL_BASE_ADDR_G : slv(31 downto 0);
      SIMULATION_G     : boolean := false );
   port (
      -- AXI-Lite Interface (axilClk domain)
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType;
      ----------------------
      -- Top Level Interface
      ----------------------
      --  SC timing input
      usRefClk        : in  sl;
      usRefClkRst     : in  sl;
      usRecClk        : in  sl;
      usRecClkRst     : in  sl;
      usRxEnable      : in  sl;
      usRx            : in  TimingRxType;
      usRxStatus      : in  TimingPhyStatusType;
      usRxControl     : out TimingPhyControlType;
      --
      timingClk       : out sl;
      timingRst       : out sl;
      timingLkN       : out sl;
      timingStream    : out XpmStreamType);
end Xpm2TimingFromUsRx;

--
--                timingClk source
--
--  usRxEnable       cuRxEnable
--                 '0'    |    '1'
--             +----------+-----------+
--      '0'    | usRefClk |  cuRxClk* |
--          ---+----------+-----------+
--      '1'    | usRecClk |  usRecClk |
--             +----------+-----------+
--

architecture mapping of Xpm2TimingFromUsRx is

   constant US_INDEX_C        : integer := 0;
   constant CU_INDEX_C        : integer := 1;
   constant SIM_INDEX_C       : integer := 2;
   constant NUM_AXI_MASTERS_C : integer := 3;

   constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(NUM_AXI_MASTERS_C-1 downto 0) :=
      genAxiLiteConfig(NUM_AXI_MASTERS_C, AXIL_BASE_ADDR_G, 24, 22);

   signal axilWriteMasters : AxiLiteWriteMasterArray(NUM_AXI_MASTERS_C-1 downto 0);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray (NUM_AXI_MASTERS_C-1 downto 0);
   signal axilReadMasters  : AxiLiteReadMasterArray (NUM_AXI_MASTERS_C-1 downto 0);
   signal axilReadSlaves   : AxiLiteReadSlaveArray (NUM_AXI_MASTERS_C-1 downto 0);
   -- Rx ports

   signal usRxMessage : TimingMessageType;
   signal usRxVector  : slv(TIMING_MESSAGE_BITS_C-1 downto 0);
   signal usRxStrobe  : sl;
   signal usRxValid   : sl;

   signal usRxExtn : TimingExtensionArray;

   signal xpmVector   : slv(XPM_MESSAGE_BITS_C-1 downto 0);
   signal xpmValid    : sl;

   signal itimingClk : sl;
   signal itimingRst : sl;
   signal itimingLkN : sl;

   signal simClk, simRst : sl;

   signal simStream   : TimingSerialType;
   signal simFiducial : sl;
   signal simSync     : sl;
   signal simLockedN  : sl;
   
   signal recStreams : TimingSerialArray(2 downto 0);

   signal txStreams   : TimingSerialArray(2 downto 0);
   signal txStreamIds : Slv4Array (2 downto 0);
   signal txAdvance   : slv (2 downto 0);
   signal txPhy       : TimingRxType;
   signal txFiducial  : sl;

begin

   timingClk <= itimingClk;
   timingRst <= itimingRst;
   timingLkN <= itimingLkN;
   
   itimingRst   <= usRecClkRst;
   itimingClk   <= usRecClk;
   itimingLkN   <= usRecClkRst;
   txStreams(0) <= recStreams(0);
   txStreams(1) <= TIMING_SERIAL_INIT_C;
   txStreams(2) <= recStreams(2);
   txFiducial   <= usRxStrobe;

   --------------------------
   -- AXI-Lite: Crossbar Core
   --------------------------  
   U_XBAR : entity surf.AxiLiteCrossbar
      generic map (
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => AXI_CROSSBAR_MASTERS_CONFIG_C'length,
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

   U_UsRx : entity lcls_timing_core.TimingRx
      generic map (
         CLKSEL_MODE_G => "LCLSII")
      port map (
         rxClk               => usRecClk,
         rxData              => usRx,
         rxControl           => usRxControl,
         rxStatus            => usRxStatus,
         timingMessage       => usRxMessage,
         timingMessageStrobe => usRxStrobe,
         timingMessageValid  => usRxValid,
         timingExtension     => usRxExtn,
         txClk               => '0',
         axilClk             => axilClk,
         axilRst             => axilRst,
         axilReadMaster      => axilReadMasters(US_INDEX_C),
         axilReadSlave       => axilReadSlaves(US_INDEX_C),
         axilWriteMaster     => axilWriteMasters(US_INDEX_C),
         axilWriteSlave      => axilWriteSlaves(US_INDEX_C));

   --
   --  Reconstructed streams
   --
   usRxVector <= toSlv(usRxMessage);

   U_UsRecSerializer : entity lcls_timing_core.WordSerializer
      generic map (
         NWORDS_G => TIMING_MESSAGE_WORDS_C)
      port map (
         txClk    => itimingClk,
         txRst    => itimingRst,
         fiducial => txFiducial,
         words    => usRxVector,
         ready    => usRxValid,
         advance  => txAdvance (0),
         stream   => recStreams(0));

   xpmVector <= usRxExtn(XPM_STREAM_ID_C).data(TIMING_EXTENSION_MESSAGE_BITS_C-1 downto
                                               TIMING_EXTENSION_MESSAGE_BITS_C-XPM_MESSAGE_BITS_C);
   xpmValid  <= usRxExtn(XPM_STREAM_ID_C).valid;

   U_XpmRecSerializer : entity lcls_timing_core.WordSerializer
      generic map (
         NWORDS_G => XPM_MESSAGE_WORDS_C)
      port map (
         txClk    => itimingClk,
         txRst    => itimingRst,
         fiducial => txFiducial,
         words    => xpmVector,
         ready    => xpmValid,
         advance  => txAdvance (2),
         stream   => recStreams (2));

   txStreamIds(0) <= toSlv(0, 4);
   txStreamIds(1) <= toSlv(CU_TIMING_STREAM_ID_C, 4);
   txStreamIds(2) <= toSlv(XPM_STREAM_ID_C, 4);

   U_SimSerializer : entity lcls_timing_core.TimingSerializer
      generic map (
         STREAMS_C => 3)
      port map (
         clk       => itimingClk,
         rst       => itimingRst,
         fiducial  => txFiducial,
         streams   => txStreams,
         streamIds => txStreamIds,
         advance   => txAdvance,
         data      => open,
         dataK     => open);

   --txPhy.decErr <= "00";
   --txPhy.dspErr <= "00";

   timingStream.fiducial <= txFiducial;
   timingStream.streams  <= txStreams;
   timingStream.advance  <= txAdvance;

end mapping;
