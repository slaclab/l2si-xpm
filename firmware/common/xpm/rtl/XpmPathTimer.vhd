-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Path Timer
--
-- Count clocks from accept to pause.
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


library surf;
use surf.StdRtlPkg.all;

library l2si;
use l2si.XpmPkg.all;

entity XpmPathTimer is
   generic (
      TPD_G : time := 1 ns);
   port (
      clk        : in  sl;
      rst        : in  sl;
      start      : in  sl;
      stop       : in  slv(MAX_DS_LINKS_C-1 downto 0);
      status     : out XpmPathTimerType);
end XpmPathTimer;

architecture rtl of XpmPathTimer is

   type RegType is record
      count  : slv(15 downto 0);
      latch  : slv(MAX_DS_LINKS_C-1 downto 0);
      status : XpmPathTimerStatusType;
   end record;
   constant REG_INIT_C : RegType := (
      count  => (others=>'0'),
      latch  => (others=>'0'),
      status => XPM_PATH_TIMER_INIT_C);

   signal r    : RegType := REG_INIT_C;
   signal r_in : RegType;

begin

   comb_p : process( r, rst, start, stop ) is
      variable v : RegType;
   begin
      v := r;

      v.count := r.count+1;

      for i in 0 to MAX_DS_LINKS_C-1 loop
         if (pause(i)='1' and r.latch(i)='0') then
            v.latch(i)           := '1';
            v.status.pathTime(i) := r.count;
         end if;
      end loop;
      
      if start = '1' or rst = '1' then
         v := REG_INIT_C;
      end if;

      r_in   <= v;
      status <= r.status;
   end process comb_p;

   seq_p : process (clk) is
   begin
      if rising_edge(clk) then
         r <= r_in after TPD_G;
      end if;
   end process seq_p;

end rtl;
