-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Pattern statistics computation
--
-- Compute statistics for events by group and coincidences.
--
-------------------------------------------------------------------------------
-- This file is part of 'L2SI Core'. It is subject to
-- the license terms in the LICENSE.txt file found in the top-level directory
-- of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'L2SI Core', including this file, may be
-- copied, modified, propagated, or distributed except according to the terms
-- contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;


library surf;
use surf.StdRtlPkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;


library l2si_core;
use l2si_core.XpmPkg.all;

library l2si;
use l2si.XpmAppPkg.all;

entity XpmPatternStats is
  generic (
    TPD_G   : time    := 1 ns );
  port (
    clk       : in  sl;
    rst       : in  sl;
    config    : in  XpmConfigType; 
    streams   : in  TimingSerialArray(2 downto 0);
    streamIds : in  Slv4Array(2 downto 0) := (x"2",x"1",x"0");
    advance   : in  slv(2 downto 0);
    fiducial  : in  sl;
    status    : out XpmPatternStatisticsType );
end XpmPatternStats;

architecture rtl of XpmPatternStats is

  type PattStateType is ( IDLE_S, EVTSEL_S, CMPSTAT_S );

  type PartRegType is record
    rateSel   : sl;
    destSel   : sl;
    seqWord   : slv(15 downto 0);
  end record;
  constant PART_REG_INIT_C : PartRegType := (
    rateSel   => '0',
    destSel   => '0',
    seqWord   => (others => '0'));

  type PartRegArray is array(natural range<>) of PartRegType;

  type RegType is record
    state     : PattStateType;
    timingBus : TimingBusType;
    part      : PartRegArray(XPM_PARTITIONS_C-1 downto 0);
    frameNum  : slv(19 downto 0);
    stats     : XpmPatternStatisticsType;
    statsL    : XpmPatternStatisticsType;
  end record;
  constant REG_INIT_C : RegType := (
    state     => IDLE_S,
    timingBus => TIMING_BUS_INIT_C,
    part      => (others => PART_REG_INIT_C),
    frameNum  => (others => '0'),
    stats     => XPM_PATTERN_STATS_INIT_C,
    statsL    => XPM_PATTERN_STATS_INIT_C);

  signal r   : RegType := REG_INIT_C;
  signal rin : RegType;

  type XpmL0SelectConfigArray is array(natural range<>) of XpmL0SelectConfigType;
  signal uconfig : XpmL0SelectConfigArray(XPM_PARTITIONS_C-1 downto 0);

  signal frame            : slv(16*TIMING_MESSAGE_WORDS_C-1 downto 0);
  signal timingBus        : TimingBusType;
  signal timingBus_strobe : sl;
  signal timingBus_valid  : sl;
  
begin

  status    <= r.statsL;
  timingBus <= r.timingBus;
  
  GEN_PART : for i in 0 to XPM_PARTITIONS_C-1 generate
    U_SYNC : entity surf.SynchronizerVector
      generic map (
        TPD_G   => TPD_G,
        WIDTH_G => 32)
      port map (
        clk                   => clk,
        dataIn(15 downto 0)   => config.partition(i).l0Select.rateSel,
        dataIn(31 downto 16)  => config.partition(i).l0Select.destSel,
        dataOut(15 downto 0)  => uconfig(i).rateSel,
        dataOut(31 downto 16) => uconfig(i).destSel);
  end generate GEN_PART;

  U_TIMING_BUS : entity lcls_timing_core.TimingSerialDelay
    generic map (
      TPD_G     => TPD_G,
      NWORDS_G  => TIMING_MESSAGE_WORDS_C,
      FDEPTH_G  => 4 )
    port map (
      clk        => clk,
      rst        => rst,
      delay      => (others=>'0'),
      fiducial_i => fiducial,
      advance_i  => advance(0),
      stream_i   => streams(0),
      frame_o    => frame,
      strobe_o   => timingBus_strobe,
      valid_o    => timingBus_valid,
      overflow_o => open);
  
  comb : process (r, rst, timingBus, uconfig, frame, timingBus_strobe, timingBus_valid) is
    variable v        : RegType;
    variable p,q      : PartRegType;
    variable u        : XpmL0StatisticsType;
    variable m        : TimingMessageType;
    variable rateSel  : sl;
    variable destSel  : sl;
    variable controlI : integer;
    variable intv     : slv(19 downto 0);
    variable i,j,k    : integer;
  begin
    v := r;

    m := timingBus.message;           -- shorthand

    case r.state is
      when IDLE_S =>
        if (timingBus.strobe = '1') then
          -- latch stats from previous second
          if timingBus.message.fixedRates(0)='1' then
            v.statsL    := r.stats;
            v.frameNum  := (others=>'0');
            v.stats     := XPM_PATTERN_STATS_INIT_C;
          end if;
          -- get control word/event codes from event
          for i in 0 to XPM_PARTITIONS_C-1 loop
            p := r.part(i);
            controlI := conv_integer(uconfig(i).rateSel(13 downto 8));
            if (controlI < m.control'length) then
              p.seqWord := m.control(controlI);
            else
              p.seqWord := (others => '0');
            end if;
            v.part(i) := p;
          end loop;
          v.state := EVTSEL_S;
        end if;

      when EVTSEL_S =>
        
        for i in 0 to XPM_PARTITIONS_C-1 loop
          p := r.part(i);
          -- calculate rateSel
          rateSel := '0';
          case uconfig(i).rateSel(15 downto 14) is
            when "00" => rateSel := m.fixedRates(conv_integer(uconfig(i).rateSel(3 downto 0)));
            when "01" =>
              if (uconfig(i).rateSel(conv_integer(m.acTimeSlot)+3-1) = '0') then
                rateSel := '0';
              else
                rateSel := m.acRates(conv_integer(uconfig(i).rateSel(2 downto 0)));
              end if;
            when "10"   => rateSel := p.seqWord(conv_integer(uconfig(i).rateSel(3 downto 0)));
            when others => rateSel := '0';
          end case;
          -- calculate destSel
          destSel := '0';
          if (uconfig(i).destSel(15) = '1' or
              ((uconfig(i).destSel(conv_integer(m.beamRequest(7 downto 4))) = '1') and
               (m.beamRequest(0) = '1'))) then
            destSel := '1';
          end if;
          p.rateSel := rateSel;
          p.destSel := destSel;
          v.part(i) := p;
        end loop;
        v.state := CMPSTAT_S;

      when CMPSTAT_S => 

        k := 0;
        for i in 0 to XPM_PARTITIONS_C-1 loop
          -- compute statistics
          u := r.stats.l0Stats(i);
          p := r.part(i);
          if (p.rateSel = '1' and p.destSel = '1') then
            u.sum := r.stats.l0Stats(i).sum + 1;
            intv := r.frameNum - u.last;
            if r.frameNum < u.first then
              u.first := r.frameNum;
            else
              if intv < u.minIntv then
                u.minIntv := intv;
              end if;
              if intv > u.maxIntv then
                u.maxIntv := intv;
              end if;
            end if;
            u.last  := r.frameNum;
          end if;
          v.stats.l0Stats(i) := u;

          -- coincidences
          for j in i to XPM_PARTITIONS_C-1 loop
            q := r.part(j);
            if (q.rateSel = '1' and q.destSel = '1' and
                p.rateSel = '1' and p.destSel = '1') then
              v.stats.l0Coinc(k) := r.stats.l0Coinc(k)+1;
            end if;
            k := k+1;
          end loop;
        end loop;

        v.frameNum := r.frameNum+1;
        v.state    := IDLE_S;

      when others => NULl;
    end case;
    
    if timingBus_strobe = '1' then
      v.timingBus.message := ToTimingMessageType(frame);
    end if;
    v.timingBus.strobe := timingBus_strobe;
    v.timingBus.valid  := timingBus_valid;

    if (rst = '1') then
      v := REG_INIT_C;
    end if;

    rin <= v;
  end process comb;

  seq : process (clk) is
  begin
    if rising_edge(clk) then
      r <= rin after TPD_G;
    end if;
  end process seq;
end rtl;
