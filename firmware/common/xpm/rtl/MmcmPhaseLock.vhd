-------------------------------------------------------------------------------
-- File       : MmcmPhaseLock.vhd
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2017-02-04
-- Last update: 2021-07-26
-------------------------------------------------------------------------------
-- Description: Application Core's Top Level
--
--
-------------------------------------------------------------------------------
-- This file is part of 'LCLS2 AMC Carrier Firmware'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'LCLS2 AMC Carrier Firmware', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
use ieee.NUMERIC_STD.all;


library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;

library unisim;
use unisim.vcomponents.all;

entity MmcmPhaseLock is
   generic (
      TPD_G             : time    := 1 ns;
      CLKIN_PERIOD_G    : real;
      DIVCLK_DIVIDE_G   : integer := 1;
      CLKOUT_DIVIDE_F_G : real;
      CLKFBOUT_MULT_F_G : real;
      CLKSYNC_DIV_G     : integer := 1;
      NUM_LOCKS_G       : integer := 1;
      ASYNC_G           : boolean := false;
      SIMULATION_G      : boolean := false);
   port (
      -- Clocks and resets
      clkIn           : in  sl;
      rstIn           : in  sl;
      clkSync         : in  sl;
      lockedSync      : in  slv(NUM_LOCKS_G-1 downto 0) := (others=>'1');
      syncIn          : in  sl;
      clkOut          : out sl;
      rstOut          : out sl;
      --
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType);
end MmcmPhaseLock;

architecture behavior of MmcmPhaseLock is

   constant CNTWID   : integer              := 6;
   constant DLYWID   : integer              := 11;
   constant HALF_PER : integer              := 28*integer(CLKOUT_DIVIDE_F_G)*CLKSYNC_DIV_G;
   constant SUMWID   : integer              := bitSize(HALF_PER)+CNTWID;
   constant SUM_PER  : slv(SUMWID downto 0) := '0' & toSlv(HALF_PER, bitSize(HALF_PER)) & toSlv(0, CNTWID);

--  constant DELAY_STEP : slv(DLYWID-1 downto 0) := toSlv(56, DLYWID);
   constant DELAY_STEP : slv(DLYWID-1 downto 0) := toSlv(1, DLYWID);
   constant DELAY_END  : slv(DLYWID-1 downto 0) := toSlv(112*integer(CLKOUT_DIVIDE_F_G), DLYWID);

   --  State of the MMCM phase shift control
   type PS_State is (IDLE_S, ACTIVE_S, STALL_S);

   --  State of the phase measurement
   type Scan_State is (RESET_S, SCAN_S, WAIT_S, DONE_S);

   type RegType is record
      delaySet       : slv(DLYWID-1 downto 0);
      delayValue     : slv(DLYWID-1 downto 0);
      minDelay       : slv(DLYWID-1 downto 0);
      maxDelay       : slv(DLYWID-1 downto 0);
      sum            : slv(SUMWID-1 downto 0);
      minSum         : slv(SUMWID-1 downto 0);
      maxSum         : slv(SUMWID-1 downto 0);
      ramwr          : sl;
      ramaddr        : slv(DLYWID-1 downto 0);
      pdState        : Scan_State;
      reset          : sl;
      resetCount     : sl;
      rstOut         : sl;
      lockBypass     : slv(NUM_LOCKS_G-1 downto 0);
      psEn           : sl;
      psIncNdec      : sl;
      psState        : PS_State;
      axilReadSlave  : AxiLiteReadSlaveType;
      axilWriteSlave : AxiLiteWriteSlaveType;
   end record;

   constant REG_INIT_C : RegType := (
      delaySet       => (others => '0'),
      delayValue     => (others => '0'),
      minDelay       => (others => '0'),
      maxDelay       => (others => '0'),
      sum            => (others => '0'),
      minSum         => (others => '1'),
      maxSum         => (others => '0'),
      ramwr          => '0',
      ramaddr        => (others => '0'),
      pdState        => RESET_S,
      reset          => '0',
      resetCount     => '1',
      rstOut         => '1',
      lockBypass     => (others=>'0'),
      psEn           => '0',
      psIncNdec      => '0',
      psState        => IDLE_S,
      axilReadSlave  => AXI_LITE_READ_SLAVE_INIT_C,
      axilWriteSlave => AXI_LITE_WRITE_SLAVE_INIT_C);

   signal r    : RegType := REG_INIT_C;
   signal r_in : RegType;

   signal count, ramdata, syncCount : slv(CNTWID-1 downto 0);
   signal ramdata1                  : slv(SUMWID-1 downto 0);
   signal isyncIn, syncOut          : sl;
   signal clkHigh                   : sl;
   signal arstIn                    : sl;
   signal lockedS                   : slv(lockedSync'range);

   signal ready : sl;

   signal clkFbIn, clkFbOut         : sl;
   signal iiclkOut, iclkOut, locked : sl;
   signal psDone                    : sl;

begin

   clkOut  <= iclkOut;
   clkFbIn <= clkFbOut;

   clkHigh <= isyncIn and clkSync;

   U_SyncStrobe : entity surf.SynchronizerOneShot
      generic map (
         TPD_G => TPD_G)
      port map (
         clk     => clkIn,
         dataIn  => syncIn,
         dataOut => isyncIn);

   U_CountStrobe : entity surf.SynchronizerOneShotCnt
      generic map (
         TPD_G       => TPD_G,
         CNT_WIDTH_G => CNTWID+1)
      port map (
         dataIn                    => syncIn,
         rollOverEn                => '0',
         cntRst                    => r.resetCount,
         dataOut                   => open,
         cntOut(CNTWID-1 downto 0) => syncCount,
         cntOut(CNTWID)            => ready,
         wrClk                     => clkIn,
         rdClk                     => axilClk);

   U_CountHigh : entity surf.SynchronizerOneShotCnt
      generic map (
         TPD_G       => TPD_G,
         CNT_WIDTH_G => CNTWID)
      port map (
         dataIn     => clkHigh,
         rollOverEn => '0',
         cntRst     => r.resetCount,
         dataOut    => open,
         cntOut     => count,
         wrClk      => clkIn,
         rdClk      => axilClk);

   U_RstSync : entity surf.RstSync
      generic map (
         TPD_G => TPD_G)
      port map (
         clk      => axilClk,
         asyncRst => rstIn,
         syncRst  => arstIn);

   U_SyncLocked : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => lockedSync'length )
      port map (
         clk     => axilClk,
         dataIn  => lockedSync,
         dataOut => lockedS);

   U_MMCM : MMCME3_ADV
      generic map (
         CLKFBOUT_MULT_F     => CLKFBOUT_MULT_F_G,
         CLKOUT0_DIVIDE_F    => CLKOUT_DIVIDE_F_G,
         DIVCLK_DIVIDE       => DIVCLK_DIVIDE_G,
         CLKIN1_PERIOD       => CLKIN_PERIOD_G,
         CLKOUT0_USE_FINE_PS => "TRUE")
      port map (
         DCLK     => axilClk,
         DADDR    => (others => '0'),
         DI       => (others => '0'),
         DWE      => '0',
         DEN      => '0',
         PSCLK    => axilClk,
         PSEN     => r.psEn,
         PSINCDEC => r.psIncNdec,
         PSDONE   => psDone,
         RST      => rstIn,
         CLKIN1   => clkIn,
         CLKIN2   => '0',
         CLKINSEL => '1',
         CLKFBOUT => clkFbOut,
         CLKFBIN  => clkFbIn,
         LOCKED   => locked,
         CLKOUT0  => iiclkOut,
         CDDCREQ  => '0',
         PWRDWN   => '0');

   U_RstOut : entity surf.RstSync
      generic map (
         TPD_G         => TPD_G,
         IN_POLARITY_G => '1')
      port map (
         clk      => iclkOut,
         asyncRst => r.rstOut,
         syncRst  => rstOut);

   U_RAM0 : entity surf.SimpleDualPortRam
      generic map (
         TPD_G        => TPD_G,
         DATA_WIDTH_G => CNTWID,
         ADDR_WIDTH_G => DLYWID)
      port map (
         clka  => axilClk,
         wea   => r.ramwr,
         addra => r.delaySet,
         dina  => count,
         clkb  => axilClk,
         addrb => r.ramaddr,
         doutb => ramdata);

   U_RAM1 : entity surf.SimpleDualPortRam
      generic map (
         TPD_G        => TPD_G,
         DATA_WIDTH_G => SUMWID,
         ADDR_WIDTH_G => DLYWID)
      port map (
         clka  => axilClk,
         wea   => r.ramwr,
         addra => r.delaySet,
         dina  => r.sum,
         clkb  => axilClk,
         addrb => r.ramaddr,
         doutb => ramdata1);

   U_BUFG : BUFG
      port map (
         I => iiclkOut,
         O => iclkOut);

   comb : process (arstIn, axilReadMaster, axilRst, axilWriteMaster, count, locked, lockedS, psDone,
                   r, ramdata, ramdata1, ready) is
      variable v  : RegType;
      variable s  : slv(SUMWID downto 0);
      variable ep : AxiLiteEndPointType;
   begin
      v := r;

      v.psEn       := '0';
      v.reset      := '0';
      v.resetCount := '0';
      v.ramwr      := '0';

      if SIMULATION_G then
         v.rstOut := '0';
      else
         case (r.pdState) is
            when RESET_S =>
               v.rstOut   := '1';
               v.delaySet := (others => '0');
               v.minDelay := (others => '0');
               v.maxDelay := (others => '0');
               v.sum      := (others => '0');
               v.minSum   := (others => '1');
               v.maxSum   := (others => '0');
               if (arstIn = '0' and locked = '1' and
                   ((lockedS and not r.lockBypass) = not r.lockBypass)) then
                  v.pdState := SCAN_S;
               end if;
               if ASYNC_G then
                  v.rstOut  := '0';
                  v.pdState := RESET_S;
               end if;
            when SCAN_S =>
               v.resetCount := '1';
               if r.delayValue = r.delaySet and ready = '0' then
                  v.pdState := WAIT_S;
               end if;
            when WAIT_S =>
               if ready = '1' then      -- new clock measurement ready
                  v.ramwr    := '1';
                  v.delaySet := r.delaySet + DELAY_STEP;
                  v.ramaddr  := v.delaySet - HALF_PER;
                  v.pdState  := SCAN_S;
                  -- Find minimum and maximum windows
                  v.sum      := r.sum + count;
                  if r.delaySet > HALF_PER then
                     if r.sum > r.maxSum then
                        v.maxSum   := r.sum;
                        v.maxDelay := r.delaySet;
                     elsif r.sum < r.minSum then
                        v.minSum   := r.sum;
                        v.minDelay := r.delaySet;
                     end if;
                     v.sum := v.sum - ramdata;
                  end if;

                  if r.delaySet = DELAY_END then
                     s := ('0' & r.minSum) + ('0' & r.maxSum);
                     if s < SUM_PER then
                        v.delaySet := v.minDelay - HALF_PER;
                     else
                        v.delaySet := v.maxDelay;
                     end if;
                     v.pdState := DONE_S;
                  end if;
               end if;
            when DONE_S =>
               if r.delayValue = r.delaySet then
                  v.rstOut := '0';
               end if;
               if r.reset = '1' then
                  v.pdState := RESET_S;
               end if;
         end case;

         case (r.psState) is
            when IDLE_S =>
               v.psEn    := '1';
               v.psState := ACTIVE_S;
               if r.delaySet > r.delayValue then
                  v.psIncNdec := '1';
               elsif r.delaySet < r.delayValue then
                  v.psIncNdec := '0';
               else
                  v.psEn    := '0';
                  v.psState := IDLE_S;
               end if;
            when ACTIVE_S =>
               if psDone = '1' then
                  if r.psIncNdec = '1' then
                     v.delayValue := r.delayValue+1;
                  else
                     v.delayValue := r.delayValue-1;
                  end if;
                  v.psState := IDLE_S;
               end if;
            when others => null;
         end case;
      end if;

      if arstIn = '1' then
         v := REG_INIT_C;
         v.lockBypass := r.lockBypass;
      end if;

      axiSlaveWaitTxn(ep, axilWriteMaster, axilReadMaster, v.axilWriteSlave, v.axilReadSlave);

      axiSlaveRegister (ep, toSlv(0, 12), 0, v.delaySet);
      axiSlaveRegister (ep, toSlv(0, 12), 16, v.lockBypass);
      axiSlaveRegisterR(ep, toSlv(4, 12), 0, r.delayValue);
      axiSlaveRegisterR(ep, toSlv(4, 12), 16, DELAY_END);
      axiSlaveRegisterR(ep, toSlv(4, 12), 29, lockedS);
      axiSlaveRegisterR(ep, toSlv(4, 12), 30, r.rstOut);
      axiSlaveRegisterR(ep, toSlv(4, 12), 31, locked);
      axiSlaveRegister (ep, toSlv(8, 12), 0, v.ramaddr);
      axiSlaveRegisterR(ep, toSlv(12, 12), 0, ramdata);
      axiSlaveRegisterR(ep, toSlv(20, 12), 0, toSlv(HALF_PER, 16));
      axiSlaveRegisterR(ep, toSlv(20, 12), 16, SUM_PER);
      axiSlaveRegisterR(ep, toSlv(24, 12), 0, r.minSum);
      axiSlaveRegisterR(ep, toSlv(24, 12), 16, r.minDelay);
      axiSlaveRegisterR(ep, toSlv(28, 12), 0, r.maxSum);
      axiSlaveRegisterR(ep, toSlv(28, 12), 16, r.maxDelay);
      axiSlaveRegisterR(ep, toSlv(32, 12), 0, ramdata1);

      axiWrDetect(ep, toSlv(16, 12), v.reset);

      axiSlaveDefault(ep, v.axilWriteSlave, v.axilReadSlave);

      if axilRst = '1' then
         v := REG_INIT_C;
      end if;
      
      r_in <= v;

      axilReadSlave  <= r.axilReadSlave;
      axilWriteSlave <= r.axilWriteSlave;
   end process;

   seq : process(axilClk) is
   begin
      if rising_edge(axilClk) then
         r <= r_in after TPD_G;
      end if;
   end process;

end behavior;
