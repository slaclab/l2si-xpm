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
use lcls_timing_core.TPGPkg.all;

library l2si_core;
use l2si_core.XpmPkg.all;

package XpmAppPkg is

   -----------------------------------------------------------
   -- Application: Configurations, Constants and Records Types
   -----------------------------------------------------------
   type XpmSeqStatusType is record
      countRequest : slv(127 downto 0);
      countInvalid : slv( 31 downto 0);
      seqRdData    : slv( 31 downto 0);
      seqState     : SequencerState;
   end record;

   constant XPM_SEQ_STATUS_INIT_C : XpmSeqStatusType := (
      countRequest => (others => '0'),
      countInvalid => (others => '0'),
      seqRdData    => (others => '0'),
      seqState     => SEQUENCER_STATE_INIT_C);

   type XpmSeqStatusArray is array(natural range<>) of XpmSeqStatusType;
     
   constant XPM_SEQ_STATUS_BITS_C : integer := 192+8*SEQCOUNTDEPTH+SEQADDRLEN;

   function toSlv(s : XpmSeqStatusType) return slv;
   function toSlv(s : XpmSeqStatusArray) return slv;
   function toXpmSeqStatusType (vector : slv) return XpmSeqStatusType;
   function toXpmSeqStatusArray (vector : slv; n : integer) return XpmSeqStatusArray;

   type XpmSeqGConfigType is record
      seqAddr       : SeqAddrType;
      seqWrData     : slv(31 downto 0);
   end record;

   constant XPM_SEQ_GCONFIG_INIT_C : XpmSeqGConfigType := (
      seqAddr       => (others => '0'),
      seqWrData     => (others => '0') );
   
   type XpmSeqConfigType is record
      seqEnable     : sl;
      seqRestart    : sl;
      seqWrEn       : sl;
      seqJumpConfig : TPGJumpConfigType;
   end record;

   constant XPM_SEQ_CONFIG_INIT_C : XpmSeqConfigType := (
      seqEnable     => '0',
      seqRestart    => '0',
      seqWrEn       => '0',
      seqJumpConfig => TPG_JUMPCONFIG_INIT_C );

   type XpmSeqConfigArray is array(natural range<>) of XpmSeqConfigType;
   
   constant XPM_SEQ_CONFIG_BITS_C : integer := 3+24+(MPSCHAN+2)*SEQADDRLEN+MPSCHAN*4;

   function toSlv(s : XpmSeqGConfigType) return slv;
   function toSlv(s : XpmSeqConfigType) return slv;
   function toSlv(s : XpmSeqConfigArray) return slv;
   function toXpmSeqGConfigType(vector : slv) return XpmSeqGConfigType;
   function toXpmSeqConfigType (vector : slv) return XpmSeqConfigType;
   function toXpmSeqConfigArray(vector : slv; n : integer) return XpmSeqConfigArray;

   type XpmSeqNotifyType is record
      valid : sl;
      addr  : SeqAddrType;
   end record;

   type XpmSeqNotifyArray is array(natural range<>) of XpmSeqNotifyType;

   type XpmL0StatisticsType is record
      sum      : slv(19 downto 0);
      first    : slv(19 downto 0);
      last     : slv(19 downto 0);
      minIntv  : slv(19 downto 0);
      maxIntv  : slv(19 downto 0);
   end record;
   constant XPM_L0_STATISTICS_INIT_C : XpmL0StatisticsType := (
      sum      => (others=>'0'),
      first    => (others=>'1'),
      last     => (others=>'1'),
      minIntv  => (others=>'1'),
      maxIntv  => (others=>'0'));
   type XpmL0StatisticsArray is array(natural range<>) of XpmL0StatisticsType;
   
   type XpmPatternStatisticsType is record
      l0Stats  : XpmL0StatisticsArray(XPM_PARTITIONS_C-1 downto 0);
      l0Coinc  : Slv20Array(XPM_PARTITIONS_C*(XPM_PARTITIONS_C+1)/2-1 downto 0);
   end record;
   constant XPM_PATTERN_STATS_INIT_C : XpmPatternStatisticsType := (
      l0Stats   => (others => XPM_L0_STATISTICS_INIT_C),
      l0Coinc   => (others => toSlv(0,20)));
   constant XPM_PATTERN_STATS_BITS_C : integer := 100*XPM_PARTITIONS_C + 20*XPM_PARTITIONS_C*(XPM_PARTITIONS_C+1)/2;
   
   function toSlv(s             : XpmPatternStatisticsType) return slv;

   type XpmPatternConfigType is record
     rateSel  : slv(15 downto 0);
   end record;

   constant XPM_PATTERN_CONFIG_INIT_C : XpmPatternConfigType := (
     rateSel  => (others=>'0') );
   
end package XpmAppPkg;

package body XpmAppPkg is

   function toSlv(s : XpmSeqStatusType) return slv
   is
     variable vector : slv(XPM_SEQ_STATUS_BITS_C-1 downto 0) := (others => '0');
     variable i      : integer                               := 0;
   begin
     assignSlv(i, vector, s.countRequest);
     assignSlv(i, vector, s.countInvalid);
     assignSlv(i, vector, s.seqRdData   );
     for k in 0 to SEQADDRLEN-1 loop
       assignSlv(i, vector, s.seqState.index(k));
     end loop;  -- k
     for k in 0 to SEQCOUNTDEPTH-1 loop
       assignSlv(i, vector, s.seqState.count(k));
     end loop;  -- k
     assert (i=XPM_SEQ_STATUS_BITS_C) report "toSlv(XpmSeqState) incomplete" severity error;
     return vector;
   end function;

   function toSlv(s : XpmSeqStatusArray) return slv
   is
     variable vector : slv(s'length*XPM_SEQ_STATUS_BITS_C-1 downto 0) := (others => '0');
     variable i      : integer                               := 0;
   begin
     for j in 0 to s'length-1 loop
       assignSlv(i, vector, toSlv(s(j)));
     end loop;
     return vector;
   end function;
   
   function toXpmSeqStatusType (vector : slv) return XpmSeqStatusType
   is
     variable s : XpmSeqStatusType;
     variable i : integer := 0;
   begin
     assignRecord(i, vector, s.countRequest);
     assignRecord(i, vector, s.countInvalid);
     assignRecord(i, vector, s.seqRdData   );
     for k in 0 to SEQADDRLEN-1 loop
       assignRecord(i, vector, s.seqState.index(k));
     end loop;
     for k in 0 to SEQCOUNTDEPTH-1 loop
       assignRecord(i, vector, s.seqState.count(k));
     end loop;  -- k
     assert (i=XPM_SEQ_STATUS_BITS_C) report "toXpmSeqState incomplete" severity error;
     return s;
   end function;

   function toXpmSeqStatusArray (vector : slv; n : integer) return XpmSeqStatusArray is
     variable s : XpmSeqStatusArray(n-1 downto 0);
     variable i : integer := 0;
   begin
     for j in 0 to n-1 loop
       s(j) := toXpmSeqStatusType( vector((j+1)*XPM_SEQ_STATUS_BITS_C-1 downto j*XPM_SEQ_STATUS_BITS_C));
     end loop;
     return s;
   end function;
   
   function toSlv(s : XpmSeqGConfigType) return slv
   is
     variable vector : slv(SEQADDRLEN+31 downto 0) := (others => '0');
     variable i      : integer                     := 0;
   begin
      for k in 0 to SEQADDRLEN-1 loop
        assignSlv(i, vector, s.seqAddr(k));
      end loop;
      assignSlv(i, vector, s.seqWrData);
      return vector;
   end function;
      
   function toSlv(s : XpmSeqConfigType) return slv
   is
     variable vector : slv(XPM_SEQ_CONFIG_BITS_C-1 downto 0) := (others => '0');
     variable i      : integer                               := 0;
   begin
      assignSlv(i, vector, s.seqEnable);
      assignSlv(i, vector, s.seqRestart);
      assignSlv(i, vector, s.seqWrEn);
      assignSlv(i, vector, s.seqWrEn);
      assignSlv(i, vector, s.seqJumpConfig.syncSel);
      assignSlv(i, vector, s.seqJumpConfig.syncClass);
      assignSlv(i, vector, s.seqJumpConfig.bcsClass);
      for k in 0 to SEQADDRLEN-1 loop
        assignSlv(i, vector, s.seqJumpConfig.syncJump(k));
        assignSlv(i, vector, s.seqJumpConfig.bcsJump(k));
      end loop;
      for k in 0 to MPSCHAN-1 loop
        for m in 0 to SEQADDRLEN-1 loop
          assignSlv(i, vector, s.seqJumpConfig.mpsJump (k)(m));
        end loop;
        assignSlv(i, vector, s.seqJumpConfig.mpsClass(k));
      end loop;  -- k
      assert (i=XPM_SEQ_CONFIG_BITS_C) report "toSlv(XpmSeqConfig) incomplete" severity error;
      return vector;
   end function;

   function toSlv(s : XpmSeqConfigArray) return slv
   is
     variable vector : slv(s'length*XPM_SEQ_CONFIG_BITS_C-1 downto 0) := (others => '0');
     variable i      : integer                                 := 0;
   begin
     for j in 0 to s'length-1 loop
       assignSlv(i, vector, toSlv(s(i)));
      end loop;
      return vector;
   end function;

   function toXpmSeqGConfigType (vector : slv) return XpmSeqGConfigType
   is
     variable s : XpmSeqGConfigType;
     variable i : integer := 0;
   begin
      for k in 0 to SEQADDRLEN-1 loop
        assignRecord(i, vector, s.seqAddr(k));
      end loop;
      assignRecord(i, vector, s.seqWrData);
      return s;
   end function;
   
   function toXpmSeqConfigType (vector : slv) return XpmSeqConfigType
   is
     variable s : XpmSeqConfigType;
     variable i : integer := 0;
   begin
      assignRecord(i, vector, s.seqEnable);
      assignRecord(i, vector, s.seqRestart);
      assignRecord(i, vector, s.seqWrEn);
      assignRecord(i, vector, s.seqJumpConfig.syncSel);
      assignRecord(i, vector, s.seqJumpConfig.syncClass);
      assignRecord(i, vector, s.seqJumpConfig.bcsClass);
      for k in 0 to SEQADDRLEN-1 loop
        assignRecord(i, vector, s.seqJumpConfig.syncJump(k));
        assignRecord(i, vector, s.seqJumpConfig.bcsJump(k));
      end loop;
      for k in 0 to MPSCHAN-1 loop
        for m in 0 to SEQADDRLEN-1 loop
          assignRecord(i, vector, s.seqJumpConfig.mpsJump (k)(m));
        end loop;
        assignRecord(i, vector, s.seqJumpConfig.mpsClass(k));
      end loop;  -- k
      assert (i=XPM_SEQ_CONFIG_BITS_C) report "toXpmSeqConfig incomplete" severity error;
      return s;
   end function;

   function toXpmSeqConfigArray (vector : slv; n : integer) return XpmSeqConfigArray is
     variable s : XpmSeqConfigArray(n-1 downto 0);
   begin
     for j in 0 to n-1 loop
       s(j) := toXpmSeqConfigType( vector((j+1)*XPM_SEQ_CONFIG_BITS_C-1 downto j*XPM_SEQ_CONFIG_BITS_C));
     end loop;
     return s;
   end function;
   
   function toSlv(s : XpmPatternStatisticsType) return slv is
      variable vector : slv(XPM_PATTERN_STATS_BITS_C-1 downto 0) := (others => '0');
      variable i,j    : integer          := 0;
   begin
     for j in 0 to XPM_PARTITIONS_C-1 loop
       assignSlv(i, vector, s.l0Stats(j).sum);
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
