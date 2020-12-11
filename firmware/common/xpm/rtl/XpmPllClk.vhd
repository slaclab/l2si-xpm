------------------------------------------------------------------------------
-- This file is part of 'LCLS2 DAQ Software'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'LCLS2 DAQ Software', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;

library unisim;
use unisim.vcomponents.all;

library l2si;

entity XpmPllClk is
   generic (
      TPD_G   : time    := 1 ns;
      ASYNC_G : boolean := false );
   port (
      clkIn           : in  sl;
      rstIn           : in  sl;
      locked          : in  slv(1 downto 0);
      clkOutP         : out slv(3 downto 0);
      clkOutN         : out slv(3 downto 0);
      clkRet          : in  sl;
      syncRst         : in  sl;
      syncIn          : in  sl;
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType);
end XpmPllClk;

architecture rtl of XpmPllClk is

   signal clkDdr   : sl := '0';
   signal clk, rst : sl;

   constant DEBUG_C : boolean := true;

   component ila_0
     port ( clk     : in sl;
            probe0  : in slv(255 downto 0) );
   end component;

   signal rstInS, lockedS, rstS : sl := '0';
   signal clkInRate : slv(31 downto 0) := (others=>'0');
   
begin

  GEN_DEBUG : if DEBUG_C generate
    U_RstInS : entity surf.Synchronizer
      port map ( clk     => axilClk,
                 dataIn  => rstIn,
                 dataOut => rstInS );
    U_LockedS : entity surf.Synchronizer
      port map ( clk     => axilClk,
                 dataIn  => locked(0),
                 dataOut => lockedS );
    U_RstS : entity surf.Synchronizer
      port map ( clk     => axilClk,
                 dataIn  => rst,
                 dataOut => rstS );
    U_ClkInRate : entity surf.SyncClockFreq
      generic map ( REF_CLK_FREQ_G => 125.0E+6,
                    COMMON_CLK_G   => true )
      port map ( freqOut => clkInRate,
                 clkIn   => clkIn,
                 locClk  => axilClk,
                 refClk  => axilClk );
    U_ILA : ila_0
      port map ( clk      => axilClk,
                 probe0(0)   => axilRst,
                 probe0(1)   => rstInS,
                 probe0(2)   => lockedS,
                 probe0(3)   => rstS,
                 probe0(35 downto  4) => clkInRate,
                 probe0(255 downto 36) => (others=>'0') );
  end generate;
                 
   --
   --  Can't use the ODDRE1 at fpgaclk_P/N(2) because it shares the
   --  BITSLICE with backplane SALT channel 4
   --
   U_FPGACLK0 : entity surf.ClkOutBufDiff
    generic map (
      XIL_DEVICE_G => "ULTRASCALE")
    port map (
      clkIn   => clk,
      clkOutP => clkOutP(0),
      clkOutN => clkOutN(0));

   U_FPGACLK2 : entity surf.ClkOutBufDiff
    generic map (
      XIL_DEVICE_G => "ULTRASCALE")
    port map (
      clkIn   => clk,
      clkOutP => clkOutP(2),
      clkOutN => clkOutN(2));

   U_MMCM : entity l2si.MmcmPhaseLock
      generic map (
         TPD_G             => TPD_G,
         CLKIN_PERIOD_G    => 5.4,
         CLKOUT_DIVIDE_F_G => 6.0,
         CLKFBOUT_MULT_F_G => 6.0,
--         CLKOUT_DIVIDE_F_G => 3.0,
--         CLKFBOUT_MULT_F_G => 3.0,
--         CLKSYNC_DIV_G     => 2,
         NUM_LOCKS_G       => locked'length,
         ASYNC_G           => ASYNC_G,
         SIMULATION_G      => false)
      port map (
         clkIn           => clkIn,
         rstIn           => syncRst,
         clkSync         => clkRet,
         lockedSync      => locked,
         syncIn          => syncIn,
         clkOut          => clk,
         rstOut          => rst,
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilWriteMaster => axilWriteMaster,
         axilWriteSlave  => axilWriteSlave,
         axilReadMaster  => axilReadMaster,
         axilReadSlave   => axilReadSlave);

   -- seq : process (clk)
   -- begin
   --    if rising_edge(clk) then
   --      clkDdr <= not clkDdr after TPD_G;
   --    end if;
   -- end process seq;

   -- -- Differential output buffer
   -- U_OBUF_0 : OBUFTDS
   --    port map (
   --       I  => clkDdr,
   --       T  => '0',
   --       O  => clkOutP(0),
   --       OB => clkOutN(0));

   -- U_OBUF_2 : OBUFTDS
   --    port map (
   --       I  => clkDdr,
   --       T  => '0',
   --       O  => clkOutP(2),
   --       OB => clkOutN(2));

   clkOutP(1) <= '0';
   clkOutN(1) <= '1';

   clkOutP(3) <= '0';
   clkOutN(3) <= '1';

end rtl;
