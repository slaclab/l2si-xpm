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

entity XpmPhase is
   generic (
      TPD_G : time := 1 ns);
   port (
      clkIn           : in  sl;         -- 186M
      syncClk         : in  sl;         -- 119M
      syncRst         : in  sl;
      syncIn          : in  sl;
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType);
end XpmPhase;

architecture rtl of XpmPhase is

   type SRegType is record
      sync : slv(7 downto 0);
   end record;

   signal sr   : SRegType := (sync => (others => '0'));
   signal srin : SRegType;

   constant CNT_FULL : integer := 16;

   type ARegType is record
      ssync  : sl;
      count  : Slv20Array(3 downto 0);
      latch  : Slv20Array(3 downto 0);
      rslave : AxiLiteReadSlaveType;
      wslave : AxiLiteWriteSlaveType;
   end record;

   constant AREG_INIT_C : ARegType := (
      ssync  => '0',
      count  => (others => (others => '0')),
      latch  => (others => (others => '0')),
      rslave => AXI_LITE_READ_SLAVE_INIT_C,
      wslave => AXI_LITE_WRITE_SLAVE_INIT_C);

   signal ar   : ARegType := AREG_INIT_C;
   signal arin : ARegType;

   signal ssync, rsync : sl;

begin

   --  Stretch the fiducial to cover the full potential delay to clkRet
   scomb : process (syncRst, sr, syncIn) is
      variable v : SRegType;
   begin
      v := sr;

      if syncIn = '1' then
         v.sync := (others => '1');
      else
         v.sync := '0' & sr.sync(sr.sync'left downto 1);
      end if;

      if syncRst = '1' then
         v.sync := (others => '0');
      end if;

      srin <= v;
   end process scomb;

   sseq : process (syncClk) is
   begin
      if rising_edge(syncClk) then
         sr <= srin after TPD_G;
      end if;
   end process sseq;

   ssync <= sr.sync(0);

   --  rsync must hold longer than ssync
   U_Strobe : entity surf.SynchronizerOneShot
      generic map (
         TPD_G         => TPD_G,
         PULSE_WIDTH_G => 16)
      port map (
         clk     => clkIn,
         dataIn  => ssync,
         dataOut => rsync);

   acomb : process (ar, axilReadMaster, axilRst, axilWriteMaster, rsync, ssync) is
      variable v  : ARegType;
      variable ep : AxiLiteEndPointType;
   begin
      v := ar;

      axiSlaveWaitTxn(ep, axilWriteMaster, axilReadMaster, v.wslave, v.rslave);

      axiSlaveRegisterR(ep, toSlv(0, 12), 0, ar.latch(0));
      axiSlaveRegisterR(ep, toSlv(4, 12), 0, ar.latch(1));
      axiSlaveRegisterR(ep, toSlv(8, 12), 0, ar.latch(2));
      axiSlaveRegisterR(ep, toSlv(12, 12), 0, ar.latch(3));

      axiSlaveDefault(ep, v.wslave, v.rslave);

      if ssync = '1' then
         if ar.ssync = '0' then
            v.count(0) := ar.count(0)+1;
            if rsync = '1' then
               v.count(2) := ar.count(2)+1;
            end if;
         else
            if rsync = '0' then
               v.count(1) := ar.count(1)+1;
            else
               v.count(3) := ar.count(3)+1;
            end if;
         end if;
      end if;

      if ar.count(0)(CNT_FULL) = '1' then
         v.latch := ar.count;
         v.count := (others => (others => '0'));
      end if;

      v.ssync := ssync;

      if axilRst = '1' then
         v := AREG_INIT_C;
      end if;

      arin <= v;

      axilReadSlave  <= ar.rslave;
      axilWriteSlave <= ar.wslave;
   end process acomb;

   aseq : process (axilClk) is
   begin
      if rising_edge(axilClk) then
         ar <= arin after TPD_G;
      end if;
   end process aseq;

end rtl;
