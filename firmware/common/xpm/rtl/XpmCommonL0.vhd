-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Raw insertion flag management
--
-- Manage the raw insertion flag for all readout groups
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
use l2si_core.XpmExtensionPkg.all;

entity XpmCommonL0 is
   generic (
     TPD_G          : time    := 1 ns;
     COMMON_DELAY_G : natural := 128
     );
   port (
      clk       : in  sl;
      rst       : in  sl;
      config    : in  XpmConfigType;
      common    : in  slv       (XPM_PARTITIONS_C-1 downto 0);
      start     : in  slv       (XPM_PARTITIONS_C-1 downto 0);
      shift     : in  sl;
      data_in   : in  Slv48Array(XPM_PARTITIONS_C-1 downto 0);
      data_out  : out slv       (XPM_PARTITIONS_C-1 downto 0) );
end XpmCommonL0;

architecture rtl of XpmCommonL0 is

  type StateType is (IDLE_S,CALC_S,ASSERT_S,READ1_S,WRITE_S,CLEAR_S,READ2_S);

  type RegType is record
    rog       : integer range 0 to XPM_PARTITIONS_C-1;
    index     : slv(7 downto 0);
    ramAddr   : slv(7 downto 0);
    ramWrEn   : sl;
    ramWrData : slv(XPM_PARTITIONS_C-1 downto 0);
    ramValue  : slv(XPM_PARTITIONS_C-1 downto 0);
    state     : StateType;
    data_out  : slv(XPM_PARTITIONS_C-1 downto 0);
  end record;
  constant REG_INIT_C : RegType := (
    rog       => 0,
    index     => (others=>'0'),
    ramAddr   => (others=>'0'),
    ramWrEn   => '0',
    ramWrData => (others=>'0'),
    ramValue  => (others=>'0'),
    state     => IDLE_S,
    data_out  => (others=>'0') );

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

   signal ramValue  : slv(XPM_PARTITIONS_C-1 downto 0);
   signal ucommon   : slv(XPM_PARTITIONS_C-1 downto 0);
  
begin

  data_out <= r.data_out;

   U_SYNC : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => common'length)
      port map (
         clk     => clk,
         dataIn  => common,
         dataOut => ucommon );
  --
  --  When a readout group is ready to generate an L0 trigger,
  --  check if the raw data retention flag was set by a prior
  --  readout group (smaller L0Delay).
  --  When a readout group sets the raw data retention flag,
  --  write it into RAM at its L0Delay offset.
  --
  U_RAM : entity surf.SimpleDualPortRam
    generic map (
      DATA_WIDTH_G => XPM_PARTITIONS_C,
      ADDR_WIDTH_G => 8
      )
    port map (
      -- Port A
      clka    => clk,
      ena     => rin.ramWrEn,
      wea     => rin.ramWrEn,
      addra   => rin.ramAddr,
      dina    => rin.ramWrData,
      -- Port B
      clkb    => clk,
      rstb    => rst,
      addrb   => rin.ramAddr,
      doutb   => ramValue );

  comb : process( r, rst, config, ucommon, start, shift, data_in, ramValue ) is
    variable v : RegType;
    variable event : XpmEventDataType;
    variable trans : XpmTransitionDataType;
  begin
    v := r;

    v.ramWrEn := '0';

    case r.state is
      when IDLE_S =>
        v.ramValue := (others=>'0');
        --  Latch all group trigger decisions
        for i in 0 to XPM_PARTITIONS_C-1 loop
          if start(i) = '1' and ucommon(i) = '0' then
            event := toXpmEventDataType(data_in(i));
            if event.valid = '1' and event.l0Accept = '1' then
              v.ramValue(i) := '1';
            end if;
          end if;
        end loop;
        if start /= 0 then
          v.state := ASSERT_S;
        end if;
      when ASSERT_S =>
        if shift = '1' then  -- transmission complete
          v.rog     := 0;
          v.ramAddr := r.index + config.partition(v.rog).pipeline.depth_fids;
          v.state   := READ1_S;
        end if;
      when READ1_S =>
        --  Add the readout group triggers to the accumulation
        v.ramWrEn   := '1';
        v.ramWrData := ramValue;
        v.ramWrData(r.rog) := r.ramValue(r.rog);
        v.state     := WRITE_S;
      when WRITE_S =>
        if r.rog=XPM_PARTITIONS_C-1 then
          v.rog     := 0;
          v.index   := r.index-1;
          --  Read the full accumulation
          v.ramAddr := r.index + toSlv(COMMON_DELAY_G,8);
          v.state   := READ2_S;
        else
          v.rog     := r.rog+1;
          v.ramAddr := r.index + config.partition(v.rog).pipeline.depth_fids;
          v.state   := READ1_S;
        end if;
      when READ2_S =>
        v.state     := IDLE_S;
        v.data_out  := ramValue;
        --  Clear the full accumulation
        v.ramWrEn   := '1';
        v.ramWrData := (others=>'0');
      when others => null;
    end case;

    if rst = '1' then
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
