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
      rslave : AxiLiteReadSlaveType;
      wslave : AxiLiteWriteSlaveType;
   end record;
   constant REG_INIT_C : RegType := (
      count  => (others=>'0'),
      latch  => (others=>'0'),
      times  => (others=>(others=>'0')),
      rslave => AXI_LITE_READ_SLAVE_INIT_C,
      wslave => AXI_LITE_WRITE_SLAVE_INIT_C);

   signal r    : RegType := REG_INIT_C;
   signal r_in : RegType;

   signal syncReadMaster  : AxiLiteReadMasterType;
   signal syncReadSlave   : AxiLiteReadSlaveType;
   signal syncWriteMaster : AxiLiteWriteMasterType;
   signal syncWriteSlave  : AxiLiteWriteSlaveType;
   
begin

  U_AXIL_XBAR : entity surf.AxiLiteAsync
    generic map (
      TPD_G     => TPD_G )
    port map (
      -- Slave Port
      sAxiClk         => axilClk,
      sAxiClkRst      => axilRst,
      sAxiReadMaster  => axilReadMaster,
      sAxiReadSlave   => axilReadSlave,
      sAxiWriteMaster => axilWriteMaster,
      sAxiWriteSlave  => axilWriteSlave,
      -- Master Port
      mAxiClk         => clk,
      mAxiClkRst      => rst,
      mAxiReadMaster  => syncReadMaster,
      mAxiReadSlave   => syncReadSlave,
      mAxiWriteMaster => syncWriteMaster,
      mAxiWriteSlave  => syncWriteSlave );
      
   comb_p : process( r, rst, start, stop, syncReadMaster, syncWriteMaster ) is
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

      axiSlaveWaitTxn(ep, syncWriteMaster, syncReadMaster, v.wslave, v.rslave);
      axiSlaveRegisterR(ep, toSlv(0, 12), 0, r.latch);
      for i in 0 to NCHAN_G-1 loop
        axiSlaveRegisterR(ep, toSlv(4*i+4, 12), 0, r.times(i));
      end loop;
      
      axiSlaveDefault(ep, v.wslave, v.rslave);

      r_in   <= v;

      syncReadSlave  <= r.rslave;
      syncWriteSlave <= r.wslave;

   end process comb_p;

   seq_p : process (clk) is
   begin
      if rising_edge(clk) then
         r <= r_in after TPD_G;
      end if;
   end process seq_p;

end rtl;
