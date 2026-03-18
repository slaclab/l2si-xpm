-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : ClkSim.vhd
-- Author     : Matt Weaver <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-10
-- Last update: 2024-10-10
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- This file is part of 'LCLS2 DAQ Software'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'LCLS2 DAQ Software', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use IEEE.NUMERIC_STD.all;
use ieee.std_logic_unsigned.all;


library surf;
use surf.StdRtlPkg.all;

library unisim;
use unisim.vcomponents.all;

entity ClkSim is
  generic ( VCO_HALF_PERIOD_G : time    := 4 ps;
            TIM_DIVISOR_G     : natural := 337;
            PHY_DIVISOR_G     : natural := 50;
            ADC_DIVISOR_G     : natural := 200 );
  port ( phyClk      : out sl;
         evrClk      : out sl;
         adcClk      : out sl);
end ClkSim;

architecture behavior of ClkSim is
  signal vco : sl;
  signal iphyClk : sl := '0';
  signal ievrClk : sl := '0';
  signal iadcClk : sl := '0';
begin

  process is
  begin
    vco <= '1';
    wait for VCO_HALF_PERIOD_G;
    vco <= '0';
    wait for VCO_HALF_PERIOD_G;
  end process;

  process (vco) is
    variable iadcCnt : integer := 0;
    variable iadcClk : sl := '0';
    variable ievrCnt : integer := 0;
    variable ievrClk : sl := '0';
    variable iphyCnt : integer := 0;
    variable iphyClk : sl := '0';
  begin
    if rising_edge(vco) then
      iadcCnt := iadcCnt + 1;
      if iadcCnt = ADC_DIVISOR_G then
        iadcCnt := 0;
        iadcClk := not iadcClk;
      end if;
      ievrCnt := ievrCnt + 1;
      if ievrCnt = TIM_DIVISOR_G then
        ievrCnt := 0;
        ievrClk := not ievrClk;
      end if;
      iphyCnt := iphyCnt + 1;
      if iphyCnt = PHY_DIVISOR_G then
        iphyCnt := 0;
        iphyClk := not iphyClk;
      end if;
    end if;
    adcClk <= iadcClk;
    evrClk <= ievrClk;
    phyClk <= iphyClk;
  end process;
end behavior;
