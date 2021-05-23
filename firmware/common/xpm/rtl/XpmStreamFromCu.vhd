-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XTPG.vhd
-- Author     : Matt Weaver  <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-11-09
-- Last update: 2021-03-31
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- This file is part of 'LCLS2 Timing Core'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'LCLS2 Timing Core', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------
library ieee;
use work.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

library surf;
use surf.StdRtlPkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;
use lcls_timing_core.TPGPkg.all;

library l2si_core;
use l2si_core.CuTimingPkg.all;

library l2si; 

--  ASYNC_G indicates the cuTiming can come at anytime but never more
--  frequent than once every two base periods, otherwise it is
--  expected to come at multiples of the base period (config.baseDivisor).
--  ASYNC_G will realign the cuTiming to the established base frequency.

entity XpmStreamFromCu is
  generic ( TPD_G   : time    := 1 ns;
            ASYNC_G : boolean := false );
  port (
    txClk      : in  sl;
    txRst      : in  sl;
    txRdy      : in  sl;
    beamCode   : in  slv(7 downto 0);
    cuTiming   : in  CuTimingType;
    cuTimingV  : in  sl;
    stream     : out TimingSerialType;
    advance    : in  sl;
    fiducial   : out sl;
    configI    : in  TPGConfigType;
    statusO    : out TPGStatusType
    );
end XpmStreamFromCu;


-- Define architecture for top level module
architecture XpmStreamFromCu of XpmStreamFromCu is

  signal frame : TimingMessageType := TIMING_MESSAGE_INIT_C;

  signal baseEnable   : sl;
  
  constant FixedRateWidth : integer := 20;
  constant FixedRateDepth : integer := lcls_timing_core.TPGPkg.FIXEDRATEDEPTH;

  signal syncReset : sl;

  signal status : TPGStatusType := TPG_STATUS_INIT_C;
  signal config : TPGConfigType;

  constant TPG_ID : integer := 0;
  
  signal istream    : TimingSerialType;
  signal istreamId  : slv(3 downto 0);
  signal iadvance   : sl;
  signal iiadvance  : sl;
  
  type RegType is record
    baseEnable      : sl;
    beamRequest     : sl;
    pulseId         : slv(63 downto 0);
    timeStamp       : slv(63 downto 0);
    dtimeStamp      : slv( 7 downto 0);
    acTimeSlot      : slv( 2 downto 0);
    acTimeSlotPhase : slv(11 downto 0);
    acRates         : slv( 5 downto 0);
    bsaInit         : slv(19 downto 0);
    bsaActive       : slv(19 downto 0);
    bsaAvgDone      : slv(19 downto 0);
    bsaDone         : slv(19 downto 0);
    control         : Slv16Array(0 to 17);
    eventcode_redux : slv(15 downto 0);
    running         : sl;
    frame           : TimingMessageType;
    cuValid         : sl;
    cuValidCount    : slv(7 downto 0);
    cuValidCountL   : slv(7 downto 0);
    cuValidPulse    : slv(11 downto 0);
    cuValidPulseL   : slv(11 downto 0);
  end record;

  constant REG_INIT_C : RegType := (
    baseEnable      => '0',
    beamRequest     => '0',
    pulseId         => (others=>'0'), 
    timeStamp       => (others=>'0'), 
    dtimeStamp      => (others=>'0'), 
    acTimeSlot      => (others=>'0'), 
    acTimeSlotPhase => (others=>'0'),
    acRates         => (others=>'0'),
    bsaInit         => (others=>'0'), 
    bsaActive       => (others=>'0'), 
    bsaAvgDone      => (others=>'0'), 
    bsaDone         => (others=>'0'),
    control         => (others=>(others=>'0')),
    eventcode_redux => (others=>'0'),
    running         => '0',
    frame           => TIMING_MESSAGE_INIT_C,
    cuValid         => '0',
    cuValidCount    => (others=>'0'),
    cuValidCountL   => (others=>'0'),
    cuValidPulse    => (others=>'0'),
    cuValidPulseL   => (others=>'0') );

  signal r   : RegType := REG_INIT_C;
  signal rin : RegType;

  constant DEBUG_C : boolean := true;

  component ila_0
    port (
      clk    : in sl;
      probe0 : in slv(255 downto 0));
  end component;
begin

  GEN_DEBUG: if DEBUG_C generate
    U_ILA : ila_0
      port map (
        clk                  => txClk,
        probe0(15 downto  0) => r.frame.pulseId(15 downto 0),
        probe0(31 downto 16) => r.frame.timeStamp(31 downto 16),
        probe0(39 downto 32) => r.frame.control(0)(7 downto 0),
        probe0(47 downto 40) => r.frame.control(2)(15 downto 8),
        probe0(48)           => baseEnable,
        probe0(49)           => syncReset,
        probe0(50)           => advance,
        probe0(51)           => istream.ready,
        probe0(67 downto 52) => istream.data,
        probe0(68)           => istream.last,
        probe0(76 downto 69) => r.cuValidCountL,
        probe0(88 downto 77) => r.cuValidPulse,
        probe0(96 downto 89) => r.dtimeStamp,
        probe0(255 downto 97) => (others=>'0') );
  end generate GEN_DEBUG;
  
  stream    <= istream;
--  streamId <= istreamId;
  iiadvance <= advance;
  fiducial  <= r.baseEnable;

  frame.pulseId         <= r.pulseId;
  frame.timeStamp       <= r.timeStamp;
  frame.acRates         <= r.acRates;
  frame.acTimeSlot      <= r.acTimeSlot;
  frame.acTimeSlotPhase <= r.acTimeSlotPhase;
  frame.beamRequest     <= toSlv(0,31) & r.beamRequest;
  frame.bsaInit         <= resize(r.bsaInit    ,64);
  frame.bsaActive       <= resize(r.bsaActive  ,64);
  frame.bsaAvgDone      <= resize(r.bsaAvgDone ,64);
  frame.bsaDone         <= resize(r.bsaDone    ,64);
  frame.control         <= r.control;

  status.pulseId        <= frame.pulseId;
  status.timeStamp      <= frame.timeStamp;

  GEN_SYNC: if not ASYNC_G generate
    syncReset <= cuTimingV;
  end generate GEN_SYNC;

  GEN_ASYNC: if ASYNC_G generate
    syncReset <= cuTimingV and not r.running;
  end generate GEN_ASYNC;
  
  BaseEnableDivider : entity l2si.SyncDivider
    generic map (
      TPD_G => TPD_G,
      Width => 16)
    port map (
      sysClk   => txClk,
      sync     => syncReset,
      enable   => '1',
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

  U_TPSerializer : entity lcls_timing_core.TPSerializer
    generic map ( TPD_G => TPD_G, Id => TPG_ID )
    port map ( txClk      => txClk,
               txRst      => txRst,
               fiducial   => baseEnable,
               msg        => r.frame,
               advance    => iiadvance ,
               stream     => istream   ,
               streamId   => istreamId );

  comb : process (txRst, r, config, baseEnable, beamCode, cuTiming, cuTimingV, frame)
    variable v          : RegType;
    variable i          : integer;
  begin  -- process
    v := r;

    v.baseEnable := baseEnable;

    i := conv_integer(beamCode(7 downto 4));
    v.eventcode_redux := cuTiming.eventCodes(16*i+15 downto 16*i);
    
    if baseEnable = '1' then
      v.running         := '1';
      v.frame           := frame;
      v.pulseId         := r.pulseId+1;
      v.pulseId(63)     := '1';
      v.timeStamp       := r.timeStamp+1;
      v.beamRequest     := '0';
      v.acRates         := (others=>'0');
      v.acTimeSlotPhase := r.acTimeSlotPhase+1;
      v.control         := (others=>(others=>'0'));
      v.bsaInit         := (others=>'0');
      v.bsaActive       := (others=>'0');
      v.bsaAvgDone      := (others=>'0');
      v.bsaDone         := (others=>'0');
      v.cuValid         := '0';
      v.cuValidCountL   := r.cuValidCount;
      v.cuValidPulse    := r.cuValidPulse+1;
    elsif r.cuValid = '1' then
      v.cuValidCount    := r.cuValidCount + 1;
    end if;

    if cuTimingV='1' then
      v.cuValid       := '1';
      v.cuValidCount  := (others=>'0');
      v.cuValidPulse  := (others=>'0');
      v.cuValidPulseL := r.cuValidPulse;
      v.timeStamp     := cuTiming.epicsTime;
      v.dtimeStamp    := cuTiming.epicsTime(7 downto 0) -
                         r.timeStamp       (7 downto 0);
      if r.eventcode_redux(conv_integer(beamCode(3 downto 0)))='1' then
        v.beamRequest := '1';
      end if;
      for i in 1 to 6 loop
        for j in 0 to 5 loop
          if cuTiming.eventCodes(10*i+1+j)='1' then
            v.acRates(j) := '1';
          end if;
        end loop;
        if cuTiming.eventCodes(10*i+1)='1' then
          v.acTimeSlot      := toSlv(i,3);
          v.acTimeSlotPhase := (others=>'0');
        end if;
      end loop;
      for i in 0 to 15 loop
        v.control(i) := cuTiming.eventCodes(16*i+15 downto 16*i);
      end loop;
      v.bsaInit         := cuTiming.bsaInit;
      v.bsaActive       := cuTiming.bsaActive;
      v.bsaAvgDone      := cuTiming.bsaAvgDone;
      v.bsaDone         := cuTiming.bsaInit;
    end if;
    
    if config.pulseIdWrEn = '1' then
      v.pulseId := config.pulseId;
      v.running := '0';
    end if;

    if txRst = '1' then
      v := REG_INIT_C;
      -- prevent reset of these registers
      v.pulseId   := r.pulseId;
      v.timeStamp := r.timeStamp;
    end if;

    rin <= v;
    
  end process;

  seq : process ( txClk )
  begin
    if rising_edge(txClk) then
      r <= rin;
    end if;
  end process;
  
  statusO <= status;
  config  <= configI;
  
end XpmStreamFromCu;
