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
use ieee.std_logic_unsigned.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;

library l2si;
use l2si.XpmPkg.all;

entity XpmPathTimer is
   generic (
     TPD_G   : time := 1 ns;
     NCHAN_G : integer := 1);
   port (
      clk             : in  sl;
      rst             : in  sl;
      start           : in  sl;
      stop            : in  slv(NCHAN_G-1 downto 0);
      --
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType);
end XpmPathTimer;

architecture rtl of XpmPathTimer is

   type RegType is record
      count  : slv(15 downto 0);
      latch  : slv(NCHAN_G-1 downto 0);
      times  : Slv16Array(NCHAN_G-1 downto 0);
   end record;
   constant REG_INIT_C : RegType := (
      count  => (others=>'0'),
      latch  => (others=>'0'),
      times  => (others=>(others=>'0')));

   signal r    : RegType := REG_INIT_C;
   signal r_in : RegType;

   type ARegType is record
      rslave : AxiLiteReadSlaveType;
      wslave : AxiLiteWriteSlaveType;
   end record;
   constant AREG_INIT_C : ARegType := (
      rslave => AXI_LITE_READ_SLAVE_INIT_C,
      wslave => AXI_LITE_WRITE_SLAVE_INIT_C);

   signal a    : ARegType := AREG_INIT_C;
   signal a_in : ARegType;

   signal latch  : slv(NCHAN_G-1 downto 0);
   signal times  : Slv16Array(NCHAN_G-1 downto 0);
   
begin

   comb_p : process( r, rst, start, stop, axilReadMaster, axilWriteMaster ) is
     variable v : RegType;
     variable ep : AxiLiteEndPointType;
   begin
      v := r;

      v.count := r.count+1;

      for i in 0 to NCHAN_G-1 loop
         if (stop(i)='1' and r.latch(i)='0') then
            v.latch(i) := '1';
            v.times(i) := r.count;
         end if;
      end loop;
      
      if start = '1' or rst = '1' then
         v := REG_INIT_C;
      end if;

      r_in   <= v;

   end process comb_p;

   seq_p : process (clk) is
   begin
      if rising_edge(clk) then
         r <= r_in after TPD_G;
      end if;
   end process seq_p;

   U_LATCH : entity surf.SynchronizerVector
     generic map (
       WIDTH_G => NCHAN_G )
     port map (
       clk      => axilClk,
       rst      => axilRst,
       dataIn   => r.latch,
       dataOut  => latch );

   GEN_CHAN : for i in 0 to NCHAN_G-1 generate
     U_TIMES : entity surf.SynchronizerVector
       generic map (
         WIDTH_G => 16 )
       port map (
         clk      => axilClk,
         rst      => axilRst,
         dataIn   => r.times(i),
         dataOut  => times(i) );
   end generate GEN_CHAN;
   
   acomb_p : process( a, axilRst, latch, times, axilReadMaster, axilWriteMaster ) is
     variable v : ARegType;
     variable ep : AxiLiteEndPointType;
   begin
      v := a;

      axiSlaveWaitTxn(ep, axilWriteMaster, axilReadMaster, v.wslave, v.rslave);
      axiSlaveRegisterR(ep, toSlv(0, 12), 0, latch);
      for i in 0 to NCHAN_G-1 loop
        axiSlaveRegisterR(ep, toSlv(4*i+4, 12), 0, times(i));
      end loop;
      
      axiSlaveDefault(ep, v.wslave, v.rslave);

      a_in <= v;

      axilReadSlave  <= a.rslave;
      axilWriteSlave <= a.wslave;
   end process acomb_p;

   aseq_p : process (axilClk) is
   begin
      if rising_edge(axilClk) then
         a <= a_in after TPD_G;
      end if;
   end process aseq_p;

end rtl;
