-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmFile.vhd
-- Author     : Matt Weaver <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-10
-- Last update: 2020-01-17
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

use STD.textio.all;
use ieee.std_logic_textio.all;


library surf;
use surf.StdRtlPkg.all;
use surf.AxiStreamPkg.all;

entity XpmFile is
  generic ( filename : string := "default.dat" );
  port ( clk     : in  sl;
         data    : in  slv(15 downto 0) );
end XpmFile;

architecture behavior of XpmFile is

begin

  process is
    function HexChar(v : in slv(3 downto 0)) return character is
      variable result : character := '0';
    begin
      case(v) is
        when x"0" => result := '0';
      when x"1" => result := '1';
      when x"2" => result := '2';
      when x"3" => result := '3';
      when x"4" => result := '4';
      when x"5" => result := '5';
      when x"6" => result := '6';
      when x"7" => result := '7';
      when x"8" => result := '8';
      when x"9" => result := '9';
      when x"A" => result := 'a';
      when x"B" => result := 'b';
      when x"C" => result := 'c';
      when x"D" => result := 'd';
      when x"E" => result := 'e';
      when x"F" => result := 'f';
      when others => null;
    end case;
    return result;
  end function;

  function HexString(v : in slv(15 downto 0)) return string is
    variable result : string(4 downto 1);
  begin
    for i in 0 to 3 loop
      result(i+1) := HexChar(v(4*i+3 downto 4*i));
    end loop;
    return result;
  end function;

  file results : text;
  variable oline : line;
  variable wordcnt : integer := 0;
  begin
    file_open(results, filename, write_mode);
    loop
      wait until rising_edge(clk);
      write(oline, HexString(data), right, 5);
      wordcnt := wordcnt+1;
      if wordcnt = 20 then
        wordcnt := 0;
        writeline(results, oline);
      end if;
    end loop;
    file_close(results);
  end process;

end behavior;
