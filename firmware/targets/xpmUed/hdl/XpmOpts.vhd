
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

library surf;
use surf.StdRtlPkg.all;

package XpmOpts is

  constant TPGMINI_C : boolean := false;
  constant XTPG_MODE           : sl := '1';
  constant US_RX_ENABLE_INIT_C : sl := '1';
  constant CU_RX_ENABLE_INIT_C : sl := '0';
  constant GEN_BP_C : boolean := false;
  
end XpmOpts;
