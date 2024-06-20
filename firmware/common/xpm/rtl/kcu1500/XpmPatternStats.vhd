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

begin

  status    <= XPM_PATTERN_STATS_INIT_C;

end rtl;
