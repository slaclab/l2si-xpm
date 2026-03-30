-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Calculates automated jumps in sequencer instruction RAM.
--   Reacts to BCS fault state change, MPS state change, and manual reset.
--   The manual reset is highest priority, followed by BCS, and MPS.
--   Any state change that isn't acted upon because of a higher priority reaction
--   will be enacted on the following cycle.
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
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

library UNISIM;
use UNISIM.VCOMPONENTS.all;

library surf;
use surf.StdRtlPkg.all;

library lcls_timing_core;
use lcls_timing_core.TPGPkg.all;

library l2si;
use l2si.XpmAppPkg.all;

entity SeqJump is
   generic (
     TPD_G     : time := 1 ns);
   port (
      -- Clock and reset
      clk      : in  sl;
      rst      : in  sl;
      config   : in  TPGJumpConfigType;
      manReset : in  sl;
      jumpEn   : in  sl;
      jumpReq  : out sl;
      jumpAddr : out slv(XPMSEQADDRLEN_C-1 downto 0)
      );
end SeqJump;

-- Define architecture for top level module
architecture mapping of SeqJump is

   type RegType is record
      config   : TPGJumpConfigType;
      manLatch : sl;
      jump     : sl;
      addr     : slv(XPMSEQADDRLEN_C-1 downto 0);
   end record;
   constant REG_INIT_C : RegType := (
      config   => TPG_JUMPCONFIG_INIT_C,
      manLatch => '0',
      jump     => '0',
      addr     => (others => '0'));

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

begin

   jumpReq  <= r.jump;
   jumpAddr <= r.addr;

   comb : process (config, jumpEn, manReset, r)
      variable v : RegType;
      variable q : slv(XPMSEQADDRLEN_C-1 downto 0);
   begin  -- process
      v      := r;
      v.jump := '0';

      if (manReset = '1' and r.manLatch = '0') then
         v.manLatch := '1';
      end if;

      --  Activate new jump if any state has changed
      if (jumpEn = '1') then
         --  Highest priority
         if (r.manLatch = '1' or manReset = '1') then
            v.jump  := '1';
            v.addr  := slv(config.syncClass) & slv(config.syncJump);
            v.manLatch := '0';
         end if;
      end if;

      --  Read in the new configuration on manual reset
      if (manReset = '1') then
         v.config := config;
      end if;

      rin <= v;
   end process comb;

   seq : process (clk) is
   begin
      if rising_edge(clk) then
         r <= rin after TPD_G;
      end if;
   end process seq;

end mapping;
