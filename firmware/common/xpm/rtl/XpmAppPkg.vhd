-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: XPM VHDL Package File
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

package XpmAppPkg is

   -----------------------------------------------------------
   -- Application: Configurations, Constants and Records Types
   -----------------------------------------------------------
   type XpmL0StatisticsType is record
      first    : slv(19 downto 0);
      last     : slv(19 downto 0);
      minIntv  : slv(19 downto 0);
      maxIntv  : slv(19 downto 0);
   end record;
   constant XPM_L0_STATISTICS_INIT_C : XpmL0StatisticsType := (
      first    => (others=>'1'),
      last     => (others=>'1'),
      minIntv  => (others=>'1'),
      maxIntv  => (others=>'0'));
   type XpmL0StatisticsArray is array(natural range<>) of XpmL0StatisticsType;
   
   type XpmPartitionStatusType is record
      inhibit  : XpmInhibitStatusType;
      l0Select : XpmL0SelectStatusType;
      l1Select : XpmL1SelectStatusType;
   end record;
   constant XPM_PARTITION_STATUS_INIT_C : XpmPartitionStatusType := (
      inhibit  => XPM_INHIBIT_STATUS_INIT_C,
      l0Select => XPM_L0_SELECT_STATUS_INIT_C,
      l1Select => XPM_L1_SELECT_STATUS_INIT_C );
   type XpmPartitionStatusArray is array(natural range<>) of XpmPartitionStatusType;

   type XpmPatternStatisticsType is record
      l0Stats  : XpmL0StatisticsArray(XPM_PARTITIONS_C-1 downto 0);
      l0Coinc  : Slv20Array(XPM_PARTITIONS_C*(XPM_PARTITIONS_C-1)/2-1 downto 0);
   end record;
   constant XPM_PATTERN_STATS_INIT_C : XpmPatternStatisticsType := (
      l0Stats   => (others => XPM_L0_STATISTICS_INIT_C),
      l0Coinc   => (others => toSlv(0,20)));
   constant XPM_PATTERN_STATS_BITS_C : integer := 80*XPM_PARTITIONS_C + 20*XPM_PARTITIONS_C*(XPM_PARTITIONS_C-1)/2;
   
   function toSlv(s             : XpmPatternStatisticsType) return slv;
                  
end package XpmAppPkg;

package body XpmAppPkg is

   function toSlv(s : XpmPatternStatisticsType) return slv is
      variable vector : slv(XPM_PATTERN_STATS_BITS_C-1 downto 0) := (others => '0');
      variable i,j    : integer          := 0;
   begin
     for j in 0 to XPM_PARTITIONS_C-1 loop
       assignSlv(i, vector, s.l0Stats(j).first);
       assignSlv(i, vector, s.l0Stats(j).last);
       assignSlv(i, vector, s.l0Stats(j).minIntv);
       assignSlv(i, vector, s.l0Stats(j).maxIntv);
     end loop;
     for j in 0 to s.l0Coinc'left loop
       assignSlv(i, vector, s.l0Coinc(j));
     end loop;
     return vector;
   end function;
      
end package body XpmAppPkg;
