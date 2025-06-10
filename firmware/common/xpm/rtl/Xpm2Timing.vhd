-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : Xpm2Timing.vhd
-- Author     : Matt Weaver <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-08
-- Last update: 2025-06-09
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

entity Xpm2Timing is
   generic (
      AXIL_BASE_ADDR_G : slv(31 downto 0);
      SIMULATION_G     : boolean := false;
      USE_XTPG_G : boolean := false;
      US_RX_ENABLE_INIT_G : boolean := true;
      CU_RX_ENABLE_INIT_G : boolean := false;
      CU_ASYNC_G          : boolean := false );
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
      usRefClk        : in  sl;         -- 186 MHz
      usRefClkRst     : in  sl;
      usRecClk        : in  sl;
      usRecClkRst     : in  sl;
      usRxEnable      : in  sl;
      usRx            : in  TimingRxType;
      usRxStatus      : in  TimingPhyStatusType;
      usRxControl     : out TimingPhyControlType;
      --  Cu timing input
      cuRefClk        : in  sl;
      cuRecClk        : in  sl;
      cuRecClkRst     : in  sl;
      cuRx            : in  TimingRxType;
      cuRxStatus      : in  TimingPhyStatusType;
      cuRxControl     : out TimingPhyControlType;
      cuSync          : out sl;
      cuRxFiducial    : out sl;
      --
      timingClk       : out sl;         -- 186 MHz
      timingRst       : out sl;
      timingLkN       : out sl;
      timingStream    : out XpmStreamType);
end Xpm2Timing;

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

architecture mapping of Xpm2Timing is

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
   signal cuRxStream       : TimingStreamType;
   signal cuFiducial       : sl;
   signal cuFiducialQ      : sl;
   signal cuValid          : sl;
   signal cuRxValid        : sl;
   signal cuRxV, cuRxVS    : slv(CU_TIMING_BITS_C-1 downto 0);

   signal usRxMessage : TimingMessageType;
   signal usRxVector  : slv(TIMING_MESSAGE_BITS_C-1 downto 0);
   signal usRxStrobe  : sl;
   signal usRxValid   : sl;

   signal usRxExtn : TimingExtensionArray;


   signal xpmVector   : slv(XPM_MESSAGE_BITS_C-1 downto 0);
   signal xpmValid    : sl;
   signal cuRecVector : slv(CU_TIMING_BITS_C-1 downto 0);
   signal cuRecValid  : sl;
   signal cuRxT       : CuTimingType := CU_TIMING_INIT_C;
   signal cuRxTS      : CuTimingType;   -- sync'd to txClk
   signal cuRxTSV     : sl;             -- valid
   signal cuRxTSVd    : sl;             -- delayed
   signal cuDelay     : slv(17 downto 0);

   signal itimingClk : sl;
   signal itimingRst : sl;
   signal itimingLkN : sl;

   signal simClk, simRst : sl;

   signal simStream   : TimingSerialType;
   signal simFiducial : sl;
   signal simSync     : sl;
   signal simLockedN  : sl;
   
   signal cuStream   : TimingSerialType;
   signal recStreams : TimingSerialArray(2 downto 0);

   signal txStreams   : TimingSerialArray(2 downto 0);
   signal txStreamIds : Slv4Array (2 downto 0);
   signal txAdvance   : slv (2 downto 0);
   signal txPhy       : TimingRxType;
   signal txFiducial  : sl;

   type RegType is record
      cuValid : sl;
      count   : slv(cuDelay'range);
   end record;

   constant REG_INIT_C : RegType := (
      cuValid => '0',
      count   => (others => '0'));

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

   type CuRegType is record
      cntFid  : slv(18 downto 0);
      cntFidL : slv(18 downto 0);
   end record;

   constant CUREG_INIT_C : CuRegType := (
      cntFid  => (others => '0'),
      cntFidL => (others => '0'));

   signal c   : CuRegType := CUREG_INIT_C;
   signal cin : CuRegType;

begin

   timingClk <= itimingClk;
   timingRst <= itimingRst;
   timingLkN <= itimingLkN;
   
   cuRxFiducial <= cuFiducial;

   --  Receive timing from upstream
   GEN_US : if (US_RX_ENABLE_INIT_G) generate
     cuSync       <= usRxStrobe and usRxMessage.fixedRates(1);
     itimingRst   <= usRecClkRst;
     itimingClk   <= usRecClk;
     itimingLkN   <= usRecClkRst;
     txStreams(0) <= recStreams(0);
     txStreams(1) <= TIMING_SERIAL_INIT_C;
     txStreams(2) <= recStreams(2);
     txFiducial   <= usRxStrobe;
   end generate;
   --  Generate timing internally
   NO_GEN_US : if (not US_RX_ENABLE_INIT_G) generate
     cuSync       <= simSync;
     itimingRst   <= simRst;
     itimingClk   <= simClk;
     itimingLkN   <= simLockedN;
     txStreams(0) <= simStream;
     txStreams(1) <= TIMING_SERIAL_INIT_C;
     txStreams(2) <= TIMING_SERIAL_INIT_C;
     txFiducial   <= simFiducial;
   end generate;

   -- GEN_CU_RX_ENABLE : if (CU_RX_ENABLE_INIT_G) generate
   --   txStreams(1) <= cuStream;
   -- end generate;

   -- GEN_CU_RX_DISABLE : if (CU_RX_ENABLE_INIT_G = false and US_RX_ENABLE_INIT_G = true) generate
   --   txStreams(1) <= recStreams(1);
   -- end generate;

   -- GEN_NO_RX_ENABLE : if (CU_RX_ENABLE_INIT_G = false and US_RX_ENABLE_INIT_G = false) generate
   --   txStreams(1) <= TIMING_SERIAL_INIT_C;
   -- end generate;

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

   U_InputSim : entity l2si.XpmInputSim
      generic map (
         AXIL_BASE_ADDR_G    => AXI_CROSSBAR_MASTERS_CONFIG_C(SIM_INDEX_C).baseAddr,
         SIMULATION_G        => SIMULATION_G,
         CU_RX_ENABLE_INIT_G => CU_RX_ENABLE_INIT_G,
         CU_ASYNC_G          => CU_ASYNC_G )
      port map (
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilWriteMaster => axilWriteMasters(SIM_INDEX_C),
         axilWriteSlave  => axilWriteSlaves(SIM_INDEX_C),
         axilReadMaster  => axilReadMasters(SIM_INDEX_C),
         axilReadSlave   => axilReadSlaves(SIM_INDEX_C),
         timingClk       => itimingClk,
         timingClkRst    => itimingRst,
         cuTiming        => cuRxTS,
         cuDelay         => cuDelay,
         cuTimingV       => cuRxTSVd,
         usRefClk        => usRefClk,
         usRefClkRst     => usRefClkRst,
         cuRecClk        => cuRecClk,
         cuRecClkRst     => cuRecClkRst,
         cuFiducial      => cuFiducialQ,
         cuFiducialIntv  => c.cntFidL,
         simClk          => simClk,
         simClkRst       => simRst,
         simLockedN      => simLockedN,
         simFiducial     => simFiducial,
         simSync         => simSync,
         simAdvance      => txAdvance(0),
         simStream       => simStream );

   U_CuRx : entity lcls_timing_core.TimingRx
      generic map (
         CLKSEL_MODE_G => "LCLSI")
      port map (
         rxClk              => cuRecClk,
         rxData             => cuRx,
         rxControl          => cuRxControl,
         rxStatus           => cuRxStatus,
         timingStreamUser   => cuRxStream,
         timingStreamStrobe => cuFiducial,
         timingStreamValid  => cuRxValid,
         txClk              => cuRefClk,
         axilClk            => axilClk,
         axilRst            => axilRst,
         axilReadMaster     => axilReadMasters(CU_INDEX_C),
         axilReadSlave      => axilReadSlaves(CU_INDEX_C),
         axilWriteMaster    => axilWriteMasters(CU_INDEX_C),
         axilWriteSlave     => axilWriteSlaves(CU_INDEX_C));

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
   --  Generated streams
   --
   cuFiducialQ                   <= cuFiducial and cuRxValid;
   cuRxT.epicsTime(63 downto 32) <= cuRxStream.dbuff.epicsTime(31 downto 0);
   cuRxT.epicsTime(31 downto 0)  <= cuRxStream.dbuff.epicsTime(63 downto 32);
   cuRxT.eventCodes              <= cuRxStream.eventCodes;
   cuRxT.bsaInit                 <= cuRxStream.dbuff.edefInit (19 downto 0);
   cuRxT.bsaDone                 <= cuRxStream.dbuff.edefMajor(29 downto 20) &
                    cuRxStream.dbuff.edefMinor(29 downto 20);
   GEN_EDEF : for i in 0 to 19 generate
      cuRxT.bsaActive (i) <= cuRxStream.dbuff.dmod(128+i) when cuRxStream.dbuff.edefInit(i) = '0' else
                             cuRxStream.dbuff.edefMinor(i);
      cuRxT.bsaAvgDone(i) <= cuRxStream.dbuff.edefAvgDn(i) when cuRxStream.dbuff.edefInit(i) = '0' else
                             cuRxStream.dbuff.edefMajor(i);
   end generate;

   cuRxV   <= toSlv(cuRxT);
   cuRxTS  <= toCuTimingType(cuRxVS);
   cuRxTSV <= cuValid;                  -- still need to delay

   U_CuSync : entity surf.SynchronizerFifo
      generic map (
         DATA_WIDTH_G => CU_TIMING_BITS_C)
      port map (
         rst    => itimingRst,
         wr_clk => cuRecClk,
         wr_en  => cuFiducial,
         din    => cuRxV,
         rd_clk => itimingClk,
         rd_en  => r.cuValid,
         valid  => cuValid,
         dout   => cuRxVS);

   U_CuStream : entity lcls_timing_core.WordSerializer
      generic map (
         NWORDS_G => CU_TIMING_WORDS_C)
      port map (
         txClk    => itimingClk,
         txRst    => itimingRst,
         fiducial => cuRxTSVd,
         words    => cuRxVS,
         ready    => '1',
         advance  => txAdvance(1),
         stream   => cuStream);

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

   cuRecVector <= usRxExtn(CU_TIMING_STREAM_ID_C).data(TIMING_EXTENSION_MESSAGE_BITS_C-1 downto
                                                       TIMING_EXTENSION_MESSAGE_BITS_C-CU_TIMING_BITS_C);
   cuRecValid  <= usRxExtn(CU_TIMING_STREAM_ID_C).valid;

   U_CuRecSerializer : entity lcls_timing_core.WordSerializer
      generic map (
         NWORDS_G => CU_TIMING_WORDS_C)
      port map (
         txClk    => itimingClk,
         txRst    => itimingRst,
         fiducial => txFiducial,
         words    => cuRecVector,
         ready    => cuRecValid,
         advance  => txAdvance (1),
         stream   => recStreams (1));

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

   comb : process (r, itimingRst, cuValid, cuDelay) is
      variable v : RegType;
   begin
      v := r;

      v.cuValid := '0';

      if cuValid = '1' then
         v.count := r.count + 1;
         if (r.count = cuDelay) then
            v.cuValid := '1';
         end if;
      else
         v.count := (others => '0');
      end if;

      if itimingRst = '1' then
         v := REG_INIT_C;
      end if;

      rin <= v;

      cuRxTSVd <= r.cuValid;
   end process comb;

   seq : process (itimingClk)
   begin
      if rising_edge (itimingClk) then
         r <= rin;
      end if;
   end process seq;

   ccomb : process (c, cuRecClkRst, cuFiducial) is
      variable v : CuRegType;
   begin
      v := c;

      v.cntFid  := c.cntFid+1;

      if cuFiducial = '1' then
         v.cntFid  := (others => '0');
         v.cntFidL := c.cntFid+1;
      end if;

      if cuRecClkRst = '1' then
         v := CUREG_INIT_C;
      end if;

      cin <= v;
   end process ccomb;

   cseq : process (cuRecClk) is
   begin
      if rising_edge(cuRecClk) then
         c <= cin;
      end if;
   end process cseq;

end mapping;
