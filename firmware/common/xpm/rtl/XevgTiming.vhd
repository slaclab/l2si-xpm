-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XevgTiming.vhd
-- Author     : Matt Weaver <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-08
-- Last update: 2021-08-03
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
use l2si_core.XpmExtensionPkg.all;
use l2si_core.XpmPkg.all;

library unisim;
use unisim.vcomponents.all;

library l2si; 

entity XevgTiming is
  generic (
    TPD_G            : time := 1 ns;
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
    -------------------------------
    -- Top Level Interface
    -------------------------------
    --  SC timing input (186 MHz)
    -------------------------------
    usRefClk        : in  sl;
    usRefClkRst     : in  sl;
    usRecClk        : in  sl;
    usRecClkRst     : in  sl;
    usRxEnable      : in  sl;
    usRx            : in  TimingRxType;
    usRxStatus      : in  TimingPhyStatusType;
    usRxSync        : out sl;
    usRxControl     : out TimingPhyControlType;
    --------------------
    --  119 MHz output
    --------------------
    timingClk       : out sl;
    timingRst       : out sl;
    timingPhy       : out TimingPhyType);
end XevgTiming;

architecture mapping of XevgTiming is

  signal usRxMessage, usRxMessageS : TimingMessageType;
  signal usRxStrobe, usRxStrobeQ, usRxStrobeS  : sl;
  signal usRxVector, usRxVectorS  : slv(TIMING_MESSAGE_BITS_C-1 downto 0);
  signal usRxValid, usRxMessageValid   : sl;

  signal usRxExtn : TimingExtensionArray;

  signal txStreams   : TimingSerialArray(0 downto 0) := (others=>TIMING_SERIAL_INIT_C);
  signal txStreamIds : Slv4Array        (0 downto 0) := (others=>x"0");
  signal txAdvance   : slv              (0 downto 0);
  signal txFiducial  : sl;

  type StateType is (S_IDLE, S_SYNC, S_FIRST, S_NEXT);
  type RegType is record
    state      : StateType;
    fifoReset  : sl;
    txFiducial : sl;
    count0     : slv(7 downto 0);
    count1     : slv(3 downto 0);
  end record;
  constant REG_INIT_C : RegType := (
    state      => S_IDLE,
    fifoReset  => '1',
    txFiducial => '0',
    count0     => (others=>'0'),
    count1     => (others=>'0') );
  signal r    : RegType := REG_INIT_C;
  signal r_in : RegType;

  constant TIME_INDEX_C      : integer := 0;
  constant MMCM0_INDEX_C     : integer := 1;
  constant MMCM1_INDEX_C     : integer := 2;
  constant MMCM2_INDEX_C     : integer := 3;
  constant NUM_AXI_MASTERS_C : integer := 4;
  constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(NUM_AXI_MASTERS_C-1 downto 0) :=
    genAxiLiteConfig(NUM_AXI_MASTERS_C, AXIL_BASE_ADDR_G, 22, 20);

  signal axilWriteMasters : AxiLiteWriteMasterArray(NUM_AXI_MASTERS_C-1 downto 0);
  signal axilWriteSlaves  : AxiLiteWriteSlaveArray (NUM_AXI_MASTERS_C-1 downto 0);
  signal axilReadMasters  : AxiLiteReadMasterArray (NUM_AXI_MASTERS_C-1 downto 0);
  signal axilReadSlaves   : AxiLiteReadSlaveArray (NUM_AXI_MASTERS_C-1 downto 0);

  signal recClkT     : slv(2 downto 0);
  signal recRstT     : slv(2 downto 0);
  signal recSync     : slv(1 downto 0);
  signal mmcmRst     : sl;
  signal itimingClk  : sl;
  signal itimingRst  : sl;
  
begin

  timingPhy.control <= TIMING_PHY_CONTROL_INIT_C;
  timingClk  <= recClkT(2);
  timingRst  <= recRstT(2);
  itimingClk <= recClkT(2);
  itimingRst <= recRstT(2);
  usRxSync   <= usRxStrobeQ;

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
      timingMessageValid  => usRxMessageValid,
      timingExtension     => usRxExtn,
      txClk               => '0',
      axilClk             => axilClk,
      axilRst             => axilRst,
      axilReadMaster      => axilReadMasters (TIME_INDEX_C),
      axilReadSlave       => axilReadSlaves  (TIME_INDEX_C),
      axilWriteMaster     => axilWriteMasters(TIME_INDEX_C),
      axilWriteSlave      => axilWriteSlaves (TIME_INDEX_C) );

  usRxVector  <= toSlv(usRxMessage);
  usRxStrobeQ <= usRxStrobe and usRxMessage.fixedRates(1);
  
  U_MMCM0 : entity l2si.MmcmPhaseLock
    generic map (
      TPD_G             => TPD_G,
      CLKIN_PERIOD_G    => 5.4,      -- ClkIn  = 185.7MHz
      CLKOUT_DIVIDE_F_G => 10.0,     -- ClkOut = 130 MHz
      CLKFBOUT_MULT_F_G => 7.0,      -- VCO    = 1300MHz
      NUM_LOCKS_G       => 1,
      SIMULATION_G      => SIMULATION_G)
    port map (
      clkIn           => usRecClk,
      rstIn           => usRecClkRst,
      clkSync         => recClkT(0),
      syncIn          => mmcmRst,
      clkOut          => recClkT(0),
      rstOut          => recRstT(0),
      axilClk         => axilClk,
      axilRst         => axilRst,
      axilWriteMaster => axilWriteMasters(MMCM0_INDEX_C),
      axilWriteSlave  => axilWriteSlaves (MMCM0_INDEX_C),
      axilReadMaster  => axilReadMasters (MMCM0_INDEX_C),
      axilReadSlave   => axilReadSlaves  (MMCM0_INDEX_C));

  U_MMCM1 : entity l2si.MmcmPhaseLock
    generic map (
      TPD_G             => TPD_G,
      CLKIN_PERIOD_G    => 7.692,    -- ClkIn  = 130MHz
      CLKOUT_DIVIDE_F_G => 13.0,     -- ClkOut =  70MHz
      CLKFBOUT_MULT_F_G => 7.0,      -- VCO    = 910MHz
      NUM_LOCKS_G       => 1,
      SIMULATION_G      => SIMULATION_G)
    port map (
      clkIn           => recClkT(0),
      rstIn           => recRstT(0),
      clkSync         => recClkT(1),
      syncIn          => mmcmRst,
      clkOut          => recClkT(1),
      rstOut          => recRstT(1),
      axilClk         => axilClk,
      axilRst         => axilRst,
      axilWriteMaster => axilWriteMasters(MMCM1_INDEX_C),
      axilWriteSlave  => axilWriteSlaves (MMCM1_INDEX_C),
      axilReadMaster  => axilReadMasters (MMCM1_INDEX_C),
      axilReadSlave   => axilReadSlaves (MMCM1_INDEX_C));

  U_MMCM2 : entity l2si.MmcmPhaseLock
    generic map (
      TPD_G             => TPD_G,
      CLKIN_PERIOD_G    => 14.3,     -- ClkIn  =  70MHz
      CLKOUT_DIVIDE_F_G => 10.0,     -- ClkOut = 119MHz
      CLKFBOUT_MULT_F_G => 17.0,     -- VCO    = 1190MHz
      NUM_LOCKS_G       => 1,
      SIMULATION_G      => SIMULATION_G)
    port map (
      clkIn           => recClkT(1),
      rstIn           => recRstT(1),
      clkSync         => recClkT(2),
      syncIn          => mmcmRst,
      clkOut          => recClkT(2),
      rstOut          => recRstT(2),
      axilClk         => axilClk,
      axilRst         => axilRst,
      axilWriteMaster => axilWriteMasters(MMCM2_INDEX_C),
      axilWriteSlave  => axilWriteSlaves (MMCM2_INDEX_C),
      axilReadMaster  => axilReadMasters (MMCM2_INDEX_C),
      axilReadSlave   => axilReadSlaves  (MMCM2_INDEX_C));

  U_SyncStrobe : entity surf.SynchronizerOneShot
    port map ( clk     => itimingClk,
               dataIn  => usRxStrobeQ,
               dataOut => usRxStrobeS );

  U_SyncFifo : entity surf.SynchronizerFifo
    generic map ( DATA_WIDTH_G => TIMING_MESSAGE_BITS_C,
                  ADDR_WIDTH_G => 4 )
    port map ( rst    => r.fifoReset,
               wr_clk => usRecClk,
               wr_en  => usRxStrobe,
               din    => usRxVector,
               rd_clk => itimingClk,
               rd_en  => r.txFiducial,
               valid  => usRxValid,
               dout   => usRxVectorS );

  U_UsRecSerializer : entity lcls_timing_core.WordSerializer
    generic map (
      NWORDS_G => TIMING_MESSAGE_WORDS_C)
    port map (
      txClk    => itimingClk,
      txRst    => r.fifoReset,
      fiducial => r.txFiducial,
      words    => usRxVectorS,
      ready    => usRxValid,
      advance  => txAdvance(0),
      stream   => txStreams(0));

  U_SimSerializer : entity lcls_timing_core.TimingSerializer
    generic map (
      STREAMS_C => 1)
    port map (
      clk       => itimingClk,
      rst       => r.fifoReset,
      fiducial  => r.txFiducial,
      streams   => txStreams,
      streamIds => txStreamIds,
      advance   => txAdvance,
      data      => timingPhy.data,
      dataK     => timingPhy.dataK);

  comb: process (r, itimingRst, usRxStrobeS ) is
    variable v : RegType;
  begin
    v := r;

    v.fifoReset  := '0';
    v.txFiducial := '0';
    v.count0     := r.count0+1;
    
    case r.state is
      when S_SYNC =>
        v.txFiducial := '1';
        v.count0     := (others=>'0');
        v.state      := S_FIRST;
      when S_FIRST =>
        if r.count0 = 129 then
          v.txFiducial := '1';
          v.count0     := (others=>'0');
          v.count1     := (others=>'0');
          v.state      := S_NEXT;
        end if;
      when S_NEXT =>
        if r.count0 = 127 then
          v.txFiducial := '1';
          v.count0     := (others=>'0');
          v.count1     := r.count1+1;
          if r.count1 = 11 then
            v.state      := S_NEXT;
          else
            v.state      := S_SYNC;
          end if;
        end if;
      when S_IDLE =>
        v.fifoReset := '1';
    end case;

    if usRxStrobeS = '1' then
      if r.state = S_IDLE then
        v.state := S_SYNC;
      else
        v.state := S_IDLE;
      end if;
    end if;
    
    if itimingRst = '1' then
      v := REG_INIT_C;
    end if;

    r_in <= v;
  end process comb;

  seq: process (itimingClk) is
  begin  -- process seq
    if rising_edge(itimingClk) then
      r <= r_in;
    end if;
  end process seq;

end mapping;
