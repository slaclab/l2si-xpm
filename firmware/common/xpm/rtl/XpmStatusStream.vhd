-----------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmStatusStream.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2021-10-14
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Software programmable register interface
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
use surf.AxiLitePkg.all;
use surf.AxiStreamPkg.all;
use surf.SsiPkg.all;

--library amc_carrier_core;
--use amc_carrier_core.AmcCarrierPkg.all;             -- ETH_AXIS_CONFIG_C

library l2si_core;
use l2si_core.XpmPkg.all;

entity XpmStatusStream is
   port (
      axilClk        : in  sl;
      axilRst        : in  sl;
      obStatusMaster : out AxiStreamMasterType;
      obStatusSlave  : in  AxiStreamSlaveType;
      --
      staClk         : in  sl;          -- timingClk
      status         : in  XpmStatusType;
      monClk         : in  slv(3 downto 0) := (others => '0'));
end XpmStatusStream;

architecture rtl of XpmStatusStream is

   type StateType is (IDLE_S, READING_S);

   constant NWORDS_C : integer := 40;

   type RegType is record
      iword  : integer range 0 to NWORDS_C;
      data   : Slv128Array(NWORDS_C-1 downto 0);
      update : slv(5 downto 0);
   end record RegType;

   constant REG_INIT_C : RegType := (
      iword  => NWORDS_C,
      data   => (others => (others => '0')),
      update => (others => '0'));

   signal r    : RegType := REG_INIT_C;
   signal r_in : RegType;

   constant STATUS_INTERVAL_C : slv(19 downto 0) := toSlv(910000-1, 20);

   type SRegType is record
      update : sl;
      cnt    : slv(STATUS_INTERVAL_C'range);
      status : XpmStatusType;
   end record;

   constant SREG_INIT_C : SRegType := (
      update => '0',
      cnt    => (others => '0'),
      status => XPM_STATUS_INIT_C);

   signal sta : SRegType := SREG_INIT_C;
   signal sin : SRegType;

   signal s : XpmStatusType;

   signal monClkRate : Slv32Array(3 downto 0);
   signal monClkLock : slv (3 downto 0);
   signal monClkSlow : slv (3 downto 0);
   signal monClkFast : slv (3 downto 0);

   function toSlv(p : XpmPartitionStatusType) is
      variable v : slv(2247 downto 0) := (others => '0');
   variable i : integer := 0;
begin
   for j in 0 to 31 loop
      assignSlv(i, vector, p.inhibit.evcounts);  -- 1024
   end loop;
   for j in 0 to 31 loop
      assignSlv(i, vector, p.inhibit.tmcounts);  -- 1024
   end loop;
   assignSlv(i, vector, p.l0Select.enabled);     -- 40
   assignSlv(i, vector, p.l0Select.inhibited);   -- 40
   assignSlv(i, vector, p.l0Select.num);         -- 40
   assignSlv(i, vector, p.l0Select.numInh);      -- 40
   assignSlv(i, vector, p.l0Select.numAcc);      -- 40
   return vector;
end function;

function toPartStatus(v : slv) is
   variable p : XpmPartitionStatusType := XPM_PARTITION_STATUS_INIT_C;
variable i : integer := 0;
begin
   for j in 0 to 31 loop
      assignRecord(i, vector, p.inhibit.evcounts(j));  -- 1024
   end loop;
   for j in 0 to 31 loop
      assignRecord(i, vector, p.inhibit.tmcounts(j));  -- 1024
   end loop;
   assignRecord(i, vector, p.l0Select.enabled);        -- 40
   assignRecord(i, vector, p.l0Select.inhibited);      -- 40
   assignRecord(i, vector, p.l0Select.num);            -- 40
   assignRecord(i, vector, p.l0Select.numInh);         -- 40
   assignRecord(i, vector, p.l0Select.numAcc);         -- 40
   return p;
end function;

signal update        : sl;
signal linkv, slinkv : Slv86Array (NDSLinks-1 downto 0);

type Slv2248VectorArray is array (natural range<>, natural range<>) of slv(2247 downto 0);
signal partv, spartv : Slv2248Array(NPartitions-1 downto 0);

begin

   GEN_MONCLK : for i in 0 to 3 generate
      U_SYNC : entity surf.SyncClockFreq
         generic map (REF_CLK_FREQ_G    => 125.00E+6,
                      CLK_LOWER_LIMIT_G => 95.0E+6,
                      CLK_UPPER_LIMIT_G => 186.0E+6)
         port map (freqOut     => monClkRate(i),
                   freqUpdated => open,
                   locked      => monClkLock(i),
                   tooFast     => monClkFast(i),
                   tooSlow     => monClkSlow(i),
                   clkIn       => monClk(i),
                   locClk      => axilClk,
                   refClk      => axilClk);
   end generate;

   GEN_LINK : for i in 0 to NDSLinks-1 generate
      linkv(i) <= toSlv(sta.status.dsLink(i));
      U_Sync : entity surf.SynchronizerVector
         generic map (WIDTH_G => linkv(0)'length)
         port map (clk     => axilClk,
                   dataIn  => linkv(i),
                   dataOut => slinkv(i));
      s.dsLink(i) <= toLinkStatus(slinkv(i));
   end generate;

   GEN_GROUP : for i in 0 to NPartitions-1 generate
      partv(i) <= toSlv(sta.status.partition(i));
      U_Sync : entity surf.SynchronizerVector
         generic map (WIDTH_G => partv(0)'length)
         port map (clk     => axilClk,
                   dataIn  => partv(i),
                   dataOut => spartv(i));
      s.partition(i) <= toPartStatus(spartv(i));
   end generate;

   GEN_LOL : for i in 0 to NAmcs-1 generate
      pll_stat(2*i+0) <= pllStatus(i).los;
      pll_stat(2*i+1) <= pllStatus(i).lol;
   end generate;

   U_StatLol : entity surf.SyncStatusVector
      generic map (
         COMMON_CLK_G => true,
         WIDTH_G      => 2*NAmcs,
         CNT_WIDTH_G  => 3)
      port map (
         statusIn     => pll_stat,
         statusOut    => pllStat,
         cntRstIn     => '0',
         rollOverEnIn => (others => '1'),
         cntOut       => pllCount,
         wrClk        => axilClk,
         rdClk        => axilClk);

   comb : process (rst, r, s, update, obStatusSlave) is
      variable v  : RegType;
      variable iw : integer;
   begin
      v := r;

      v.update       := r.update(r.update'left downto 1) & update;
      v.master.tKeep := genTKeep(16);
      ssiSetUserEofe(EMAC_AXIS_CONFIG_C, v.master, '0');

      if obStatusSlave.tReady = '1' then
         v.master.tValid := '0';
      end if;

      --
      --  Define the packet format
      --    This is either a large combinatoric block, or it's the load phase
      --    of a large shift register
      --

      if r.update(0) = '1' then
         v.iword := 0;

         iw := 0;
         for i in 0 to 6 loop
            v.data(iw)(16*i+15 downto 16*i) := s.dsLink(i).rxErrCnts;
            v.data(iw) (i+112)              := s.dsLink(i).txResetDone;
            v.data(iw) (i+120)              := s.dsLink(i).txReady;
         end loop;
         iw := iw+1;
         for i in 0 to 6 loop
            v.data(iw)(16*i+15 downto 16*i) := s.dsLink(i+7).rxErrCnts;
            v.data(iw) (i+112)              := s.dsLink(i+7).txResetDone;
            v.data(iw) (i+120)              := s.dsLink(i+7).txReady;
         end loop;
         iw := iw+1;
         for i in 0 to 3 loop
            v.data(iw)(32*i+31 downto 32*i) := s.dsLink(i).rxRcvCnts;
         end loop;
         iw := iw+1;
         for i in 0 to 2 loop
            v.data(iw)(32*i+31 downto 32*i) := s.dsLink(i+4).rxRcvCnts;
         end loop;
         iw := iw+1;
         for i in 0 to 6 loop
            v.data(iw) (i+ 96) := s.dsLink(i).rxResetDone;
            v.data(iw) (i+104) := s.dsLink(i).rxReady;
            v.data(iw) (i+112) := s.dsLink(i).rxIsXpm;
         end loop;
         iw := iw+1;
         for i in 0 to 3 loop
            v.data(iw)(32*i+31 downto 32*i) := s.dsLink(i+7).rxRcvCnts;
         end loop;
         iw := iw+1;
         for i in 0 to 2 loop
            v.data(iw)(32*i+31 downto 32*i) := s.dsLink(i+11).rxRcvCnts;
         end loop;
         for i in 0 to 6 loop
            v.data(iw) (i+ 96) := s.dsLink(i+7).rxResetDone;
            v.data(iw) (i+104) := s.dsLink(i+7).rxReady;
            v.data(iw) (i+112) := s.dsLink(i+7).rxIsXpm;
         end loop;
         iw := iw+1;
         for i in 0 to 3 loop
            v.data(iw)(32*i+31 downto 32*i) := s.dsLink(i).rxId;
         end loop;
         iw := iw+1;
         for i in 0 to 2 loop
            v.data(iw)(32*i+31 downto 32*i) := s.dsLink(i+4).rxId;
         end loop;
         iw := iw+1;
         for i in 0 to 3 loop
            v.data(iw)(32*i+31 downto 32*i) := s.dsLink(i+7).rxId;
         end loop;
         iw := iw+1;
         for i in 0 to 2 loop
            v.data(iw)(32*i+31 downto 32*i) := s.dsLink(i+11).rxId;
         end loop;
         iw := iw+1;
         for i in 0 to 7 loop
            v.data(iw)(39 downto 0)   := s.partition(i).l0Select.enabled;
            v.data(iw)(79 downto 40)  := s.partition(i).l0Select.inhibited;
            v.data(iw)(119 downto 80) := s.partition(i).l0Select.num;
            iw                        := iw+1;
            v.data(iw)(39 downto 0)   := s.partition(i).l0Select.numInh;
            v.data(iw)(79 downto 40)  := s.partition(i).l0Select.numAcc;
            iw                        := iw+1;
         end loop;
         for j in 0 to 13 loop
            for i in 0 to 1 loop
               v.data(iw)(31 downto 0)   := s.partition(4*i+0).inhibit.evcounts(j);
               v.data(iw)(63 downto 32)  := s.partition(4*i+0).inhibit.tmcounts(j);
               v.data(iw)(95 downto 64)  := s.partition(4*i+1).inhibit.evcounts(j);
               v.data(iw)(127 downto 96) := s.partition(4*i+1).inhibit.tmcounts(j);
               iw                        := iw+1;
               v.data(iw)(31 downto 0)   := s.partition(4*i+2).inhibit.evcounts(j);
               v.data(iw)(63 downto 32)  := s.partition(4*i+2).inhibit.tmcounts(j);
               v.data(iw)(95 downto 64)  := s.partition(4*i+3).inhibit.evcounts(j);
               v.data(iw)(127 downto 96) := s.partition(4*i+3).inhibit.tmcounts(j);
               iw                        := iw+1;
            end loop;
         end loop;

         for i in 0 to 3 loop
            v.data(iw)(32*i+31 downto 32*i) := monClkLock(i) & monClkFast(i) & monClkSlow(i) &
                                               monClkRate(i)(28 downto 0);
         end loop;
         iw := iw+1;

         for i in 0 to 3 loop
            v.data(iw)(4*i+3 downto 4*i) := pllStat(0) & muxSlVectorArray(pllCount, i);
         end loop;
         iw := iw+1;

      elsif v.master.tValid = '0' and r.iword < r.data'length then
         v.iword         := r.iword + 1;
         v.master.tValid := '1';
         v.master.tData  := r.data(0);
         v.master.tLast  := ite(v.iword = NWORD_C, '1', '0');
         ssiSetUserSof (EMAC_AXIS_CONFIG_C, v.master, ite(r.iword = 0, '1', '0'));
         v.data          := toSlv(0, 128) & r.data(r.data'left downto 1);
      end if;

      ----------------------------------------------------------------------------------------------
      -- Reset
      ----------------------------------------------------------------------------------------------
      if (axilRst = '1') then
         v := REG_INIT_C;
      end if;

      r_in <= v;
   end process;

   seq : process (axilClk) is
   begin
      if rising_edge(axilClk) then
         r <= r_in;
      end if;
   end process;

   rcomb : process (axilRst, sta, status) is
      variable v : SRegType;
   begin
      v        := sta;
      v.update := '0';
      v.cnt    := r.cnt + 1;
      if sta.cnt = STATUS_INTERVAL_C then
         v.update := '1';
         v.cnt    := (others => '0');
         v.status := status;
      end if;

      if axilRst = '1' then
         v := SREG_INIT_C;
      end if;

      sin <= v;
   end process rcomb;

   rseq : process (staClk) is
   begin
      if rising_edge(staClk) then
         sta <= sin;
      end if;
   end process rseq;

end rtl;
