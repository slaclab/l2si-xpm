-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XTPG.vhd
-- Author     : Matt Weaver  <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-11-09
-- Last update: 2019-10-28
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

entity XpmStreamFromCu is
  generic ( TPD_G : time := 1 ns );
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
  signal cuTimingEdge : sl;
  
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
    cuTimingV       : sl;
    beamRequest     : sl;
    pulseId         : slv(63 downto 0);
    timeStamp       : slv(63 downto 0);
    acTimeSlot      : slv( 2 downto 0);
    acTimeSlotPhase : slv(11 downto 0);
    acRates         : slv( 5 downto 0);
    bsaInit         : slv(19 downto 0);
    bsaActive       : slv(19 downto 0);
    bsaAvgDone      : slv(19 downto 0);
    bsaDone         : slv(19 downto 0);
    control         : Slv16Array(0 to 17);
    eventcode_redux : slv(15 downto 0);
  end record;

  constant REG_INIT_C : RegType := (
    baseEnable      => '0',
    cuTimingV       => '0',
    beamRequest     => '0',
    pulseId         => (others=>'0'), 
    timeStamp       => (others=>'0'), 
    acTimeSlot      => (others=>'0'), 
    acTimeSlotPhase => (others=>'0'),
    acRates         => (others=>'0'),
    bsaInit         => (others=>'0'), 
    bsaActive       => (others=>'0'), 
    bsaAvgDone      => (others=>'0'), 
    bsaDone         => (others=>'0'),
    control         => (others=>(others=>'0')),
    eventcode_redux => (others=>'0') );

  signal r   : RegType := REG_INIT_C;
  signal rin : RegType;
  
begin

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
  
  BaseEnableDivider : entity l2si.SyncDivider
    generic map (
      TPD_G => TPD_G,
      Width => 16)
    port map (
      sysClk   => txClk,
      sync     => cuTimingV,
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
               msg        => frame,
               advance    => iiadvance ,
               stream     => istream   ,
               streamId   => istreamId );

  comb : process (txRst, r, config, baseEnable, beamCode, cuTiming, cuTimingV)
    variable v          : RegType;
    variable i          : integer;
  begin  -- process
    v := r;

    v.baseEnable := baseEnable;
    v.cuTimingV  := cuTimingV;
    
    i := conv_integer(beamCode(7 downto 4));
    v.eventcode_redux := cuTiming.eventCodes(16*i+15 downto 16*i);
    
    if baseEnable = '1' then
      v.pulseId         := r.pulseId+1;
      v.pulseId(63)     := '1';
      v.beamRequest     := '0';
      v.acRates         := (others=>'0');
      v.acTimeSlotPhase := r.acTimeSlotPhase+1;
      v.control         := (others=>(others=>'0'));
      v.bsaInit         := (others=>'0');
      v.bsaActive       := (others=>'0');
      v.bsaAvgDone      := (others=>'0');
      v.bsaDone         := (others=>'0');
      if r.cuTimingV = '1' then
        v.timeStamp     := cuTiming.epicsTime;
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
    end if;

    if config.pulseIdWrEn = '1' then
      v.pulseId := config.pulseId;
    end if;
    
    if txRst = '1' then
      v := REG_INIT_C;
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
