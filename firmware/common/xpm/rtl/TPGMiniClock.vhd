------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- This file is part of 'LCLS Timing Core'.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'LCLS Timing Core', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

library surf;
use surf.StdRtlPkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;
use lcls_timing_core.TPGPkg.all;

entity TPGMiniClock is
  generic (
    TPD_G : time := 1 ns;
    STREAM_INTF  : boolean := false;
    AC_PERIOD    : integer := 2574
    );
  port (
    txClk           : in  sl;
    txRst           : in  sl;
    txRdy           : in  sl;
    config          : in  TPGConfigType;
    status          : out TPGStatusType;
    txData          : out slv(15 downto 0);
    txDataK         : out slv(1 downto 0);
    -- alternate output (STREAM_INTF=true)
    streams         : out TimingSerialArray(0 downto 0);
    streamIds       : out Slv4Array        (0 downto 0);
    advance         : in  slv              (0 downto 0) := (others=>'0');
    fiducial        : out sl );
end TPGMiniClock;


-- Define architecture for top level module
architecture TPGMiniClock of TPGMiniClock is

  type RegType is record
    pulseId     : slv(63 downto 0);
    count360    : slv(11 downto 0);
    countAC     : Slv8Array(ACRATEDEPTH-1 downto 0);
    timeSlot    : slv( 2 downto 0);
    acRates     : slv( 5 downto 0);
    acRatesL    : slv( 5 downto 0);
    baseEnabled : slv( 4 downto 0);
  end record;
  constant REG_INIT_C : RegType := (
    pulseId     => (others=>'0'),
    count360    => (others=>'0'),
    countAC     => (others=>toSlv(1,8)),
    timeSlot    => toSlv(1,3),
    acRates     => (others=>'0'),
    acRatesL    => (others=>'0'),
    baseEnabled => (others=>'0') );

  signal r   : RegType := REG_INIT_C;
  signal rin : RegType;

  signal frame : TimingMessageType := TIMING_MESSAGE_INIT_C;

  signal baseEnable  : sl;
  signal baseEnabled : slv(4 downto 0);

  constant ACRateWidth : integer := 8;
  constant ACRateDepth : integer := lcls_timing_core.TPGPkg.ACRATEDEPTH;

  constant FixedRateWidth : integer := 20;
  constant FixedRateDepth : integer := lcls_timing_core.TPGPkg.FIXEDRATEDEPTH;

  signal syncReset : sl;

  constant TPG_ID : integer := 0;

  signal istreams   : TimingSerialArray(0 downto 0);
  signal istreamIds : Slv4Array(0 downto 0);
  signal iadvance   : slv(0 downto 0);
  signal iiadvance  : slv(0 downto 0);

begin

  streams   <= istreams;
  streamIds <= istreamIds;
  iiadvance <= advance  when STREAM_INTF=true else
               iadvance;
  fiducial  <= baseEnable;

  -- Dont know about these inputs yet
  frame.bcsFault <= (others => '0');

  frame.mpsLimit       <= (others => '0');
  frame.mpsClass       <= (others => (others => '0'));
  frame.mpsValid       <= '0';

  syncReset        <= '0';
  frame.resync     <= '0';
  frame.syncStatus <= '0';

  BaseEnableDivider : entity lcls_timing_core.Divider
    generic map (
      TPD_G => TPD_G,
      Width => 16)
    port map (
      sysClk   => txClk,
      sysReset => syncReset,
      enable   => '1',
      clear    => '0',
      divisor  => config.baseDivisor,
      trigO    => baseEnable);

  FixedDivider_loop : for i in 0 to FixedRateDepth-1 generate
    U_FixedDivider_1 : entity lcls_timing_core.Divider
      generic map (
        TPD_G => TPD_G,
        Width => FixedRateWidth)
      port map (
        sysClk   => txClk,
        sysReset => txRst,
        enable   => baseEnable,
        clear    => '0',
        divisor  => config.FixedRateDivisors(i),
        trigO    => frame.fixedRates(i));
  end generate FixedDivider_loop;

  frame.beamRequest <= (others=>'0');

  frame.bsaInit     <= (others=>'0');
  frame.bsaActive   <= (others=>'0');
  frame.bsaAvgDone  <= (others=>'0');
  frame.bsaDone     <= (others=>'0');

  U_TSerializer : entity lcls_timing_core.TimingSerializer
    generic map ( TPD_G => TPD_G, STREAMS_C => 1 )
    port map ( clk       => txClk,
               rst       => txRst,
               fiducial  => baseEnabled(0),
               streams   => istreams,
               streamIds => istreamIds,
               advance   => iadvance,
               data      => txData,
               dataK     => txDataK );

  U_TPSerializer : entity lcls_timing_core.TPSerializer
    generic map ( TPD_G => TPD_G, Id => TPG_ID )
    port map ( txClk      => txClk,
               txRst      => txRst,
               fiducial   => baseEnable,
               msg        => frame,
               advance    => iiadvance (0),
               stream     => istreams  (0),
               streamId   => istreamIds(0) );

  comb: process(r, txRst, baseEnable, config) is
    variable v : RegType;
    variable itimeslot, ctimeslot : integer;
    variable ecode : integer;
  begin
    v := r;

    v.baseEnabled := r.baseEnabled(r.baseEnabled'left-1 downto 0) & baseEnable;
    
    if config.pulseIdWrEn = '1' then
      v.pulseId := config.pulseId;
    end if;
    
    if baseEnable = '1' then
      v.pulseId     := r.pulseId + 1;
      v.count360    := r.count360 + 1;

      v.acRates     := (others=>'0');
      if r.count360=AC_PERIOD-1 then
        v.count360 := (others=>'0');
        v.timeSlot := r.timeSlot + 1;
        if r.timeSlot=6 then
          v.timeSlot := toSlv(1,3);
          v.acRatesL := (others=>'0');
          for i in 0 to ACRATEDEPTH-1 loop
            if r.countAC(i) = config.ACRateDivisors(i) then
              v.countAC (i) := toSlv(1,8);
              v.acRatesL(i) := '1';
            else
              v.countAC (i) := r.countAC(i) + 1;
            end if;
          end loop;
        end if;
        v.acRates := v.acRatesL;
      end if;
    end if;
    
    if txRst='1' then
      v := REG_INIT_C;
    end if;

    rin <= v;

    baseEnabled       <= r.baseEnabled;
    frame.pulseId     <= r.pulseId;
    frame.acRates     <= r.acRates;
    frame.acTimeSlot  <= r.timeSlot;
    frame.acTimeSlotPhase <= r.count360;

    -- Assert the LCLS1 event codes
    frame.control     <= (others=>(others=>'0'));
    itimeslot := conv_integer(r.timeSlot);
    for i in 0 to 5 loop
      if r.acRates(i)='1' then
        ecode := 10*itimeslot + 6-i;
        frame.control(ecode/16)(ecode mod 16) <= '1';
      end if;
    end loop;
    if r.acRates(5) = '1' then
      case itimeslot is
        when 1 | 4 =>
          frame.control(0)(10) <= '1';
          frame.control(2)( 8) <= '1';
        when 2 | 5 =>
          frame.control(1)( 4) <= '1';
          frame.control(3)( 2) <= '1';
        when 3 | 6 =>
          frame.control(1)(14) <= '1';
          frame.control(3)(12) <= '1';
        when others =>
          null;
      end case;       
    end if;
    
  end process;

  seq: process (txClk) is
  begin
    if rising_edge(txClk) then
      r <= rin after TPD_G;
    end if;
  end process;

  U_ClockTime : entity lcls_timing_core.ClockTime
    generic map (
      TPD_G =>   TPD_G)
    port map (
      rst       => txRst,
      clkA      => txClk,
      wrEnA     => config.timeStampWrEn,
      wrData    => config.timeStamp,
      rdData    => status.timeStamp,
      clkB      => txClk,
      wrEnB     => baseEnable,
      step      => config.clock_step     (4 downto 0),
      remainder => config.clock_remainder(4 downto 0),
      divisor   => config.clock_divisor  (4 downto 0),
      dataO     => frame.timeStamp);

end TPGMiniClock;
