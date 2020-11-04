-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : xpm_sim.vhd
-- Author     : Matt Weaver <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-10
-- Last update: 2020-10-21
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: XpmApp's Top Level
-- 
-- Note: Common-to-XpmApp interface defined here (see URL below)
--       https://confluence.slac.stanford.edu/x/rLyMCw
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
use ieee.std_logic_unsigned.all;


library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;
use surf.AxiStreamPkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;
use lcls_timing_core.TPGPkg.all;
use lcls_timing_core.TPGMiniEdefPkg.all;

library l2si_core;
use l2si_core.XpmPkg.all;
use l2si_core.XpmMiniPkg.all;

library l2si;

--use work.AxiLiteSimPkg.all;

library unisim;
use unisim.vcomponents.all;

entity xpm_sim is
end xpm_sim;

architecture top_level_app of xpm_sim is

   constant NDsLinks : integer := 14;
  
   signal tpgConfig : TPGConfigType := TPG_CONFIG_INIT_C;
   signal xpmConfig : XpmMiniConfigType := XPM_MINI_CONFIG_INIT_C;
   
   signal regClk         : sl;
   signal regRst         : sl;

   signal dsClk, dsRst : slv(NDsLinks-1 downto 0);
   signal dsRx       : TimingRxArray (NDsLinks-1 downto 0) := (others=>TIMING_RX_INIT_C);
   signal dsTx       : TimingPhyArray(NDsLinks-1 downto 0) := (others=>TIMING_PHY_INIT_C);

   signal scClk, scRst : sl;
   signal usClk, usRst : sl;
   signal usRx  : TimingRxType := TIMING_RX_INIT_C;
   signal usRxStatus : TimingPhyStatusType := TIMING_PHY_STATUS_INIT_C;

   signal xpmStream  : XpmStreamType := XPM_STREAM_INIT_C;
   signal xData      : TimingRxType := TIMING_RX_INIT_C;
   signal xDataStatus: TimingPhyStatusType := TIMING_PHY_STATUS_INIT_C;

   signal tpgStream   : TimingSerialType;
   signal tpgAdvance  : sl;
   signal tpgFiducial : sl;
   
   type RegType is record
     advance : sl;
   end record;

   constant REG_INIT_C : RegType := ( advance => '0' );

   signal r    : RegType := REG_INIT_C;
   signal rin  : RegType;

   signal pllCount : SlVectorArray(3 downto 0,2 downto 0) := (others=>(others=>'0'));
   signal pllStat  : slv(3 downto 0) := "1011";
   signal monClkRate : Slv32Array(3 downto 0) := ( toSlv(3,32), toSlv(2,32), toSlv(1,32), toSlv(0,32));
   signal status     : XpmStatusType := XPM_STATUS_INIT_C;
   signal obMonitorMaster : AxiStreamMasterType;
   signal obMonitorSlave  : AxiStreamSlaveType := AXI_STREAM_SLAVE_FORCE_C;
   
begin

   xpmConfig.partition.l0Select.enabled <= '1';
   xpmConfig.partition.l0Select.rateSel <= x"0001";
   xpmConfig.partition.l0Select.destSel <= x"8000";
   xpmConfig.partition.pipeline.depth_fids <= toSlv(2,8);
   xpmConfig.partition.pipeline.depth_clks <= toSlv(400,16);
     
   process is
   begin
     regRst <= '1';
     xpmConfig.partition.l0Select.reset <= '1';
     wait for 100 ns;
     regRst <= '0';
     xpmConfig.partition.l0Select.reset <= '0';
     tpgConfig.pulseIdWrEn <= '0';
     wait;
   end process;
   
   process is
   begin
     regClk <= '1';
     wait for 4.0 ns;
     regClk <= '0';
     wait for 4.0 ns;
   end process;
     
   process is
   begin
     scClk <= '1';
     wait for 2.69 ns;
     scClk <= '0';
     wait for 2.69 ns;
   end process;
   scRst <= regRst;

   usClk <= not scClk;
   usRst <= scRst;

   dsClk <= (others=>usClk);
   dsRst <= (others=>usRst);

   U_UsSim : entity lcls_timing_core.TPGMini
     generic map ( NARRAYSBSA => 0,
                   STREAM_INTF => true )
     port map ( statusO   => open,
                configI   => tpgConfig,
                --
                txClk     => usClk,
                txRst     => usRst,
                txRdy     => '1',
                streams(0)=> tpgStream,
                advance(0)=> tpgAdvance,
                fiducial  => tpgFiducial );

   xpmStream.fiducial   <= tpgFiducial;
   xpmStream.advance(0) <= tpgAdvance;
   xpmStream.streams(0) <= tpgStream;

   U_Application : entity l2si_core.XpmMini
     generic map (
       NUM_DS_LINKS_G  => NDsLinks )
     port map (
       regclk          => regClk,
       regrst          => regRst,
       update          => '1',
       status          => open,
       config          => xpmConfig,
       -- DS Ports
       dsRxClk         => dsClk,
       dsRxRst         => dsRst,
       dsRx            => dsRx,
       dsTx            => dsTx,
       -- Timing Interface (timingClk domain) 
       timingClk       => usClk,
       timingRst       => usRst,
       timingStream    => xpmStream );

--   U_WriteX : entity work.XpmFile
--     generic map ( filename => "xpmmini.dat" )
--     port map ( clk  => dsClk(0),
--                data => dsTx(0).data );

   comb : process ( r, usRst, tpgFiducial, tpgStream ) is
     variable v : RegType;
   begin
     v := r;

     v.advance := tpgStream.ready and not tpgFiducial;
     
     if usRst = '1' then
       v := REG_INIT_C;
     end if;

     rin <= v;

     tpgAdvance <= v.advance;
   end process;

   seq : process ( usClk )
   begin
     if rising_edge(usClk) then
       r <= rin;
     end if;
   end process;

   U_MONSTREAM : entity l2si.XpmMonitorStream
     port map (
      axilClk         => regClk,
      axilRst         => regRst,
      enable          => '1',
      period          => toSlv(1000,27),
      pllCount        => pllCount,
      pllStat         => pllStat,
      monClkRate      => monClkRate,
      status          => status,
      staClk          => usClk,
      -- Application Debug Interface (sysclk domain)
      obMonitorMaster => obMonitorMaster,
      obMonitorSlave  => obMonitorSlave );
   
end top_level_app;
