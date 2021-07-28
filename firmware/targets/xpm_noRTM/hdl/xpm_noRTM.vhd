-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : xpm_noRTM.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2020-11-04
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Firmware Target's Top Level
-- 
-- Note: Common-to-Application interface defined here (see URL below)
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
use ieee.std_logic_arith.all;

library unisim;
use unisim.vcomponents.all;

library surf;
use surf.StdRtlPkg.all;

library l2si;

entity xpm_noRTM is
   generic (
      TPD_G         : time    := 1 ns;
      BUILD_INFO_G  : BuildInfoType);
   port (
      -----------------------
      -- Application Ports --
      -----------------------
      -- AMC's HS Ports
      dsClkP      : in    Slv1Array(1 downto 0);
      dsClkN      : in    Slv1Array(1 downto 0);
      dsRxP       : in    Slv7Array(1 downto 0);
      dsRxN       : in    Slv7Array(1 downto 0);
      dsTxP       : out   Slv7Array(1 downto 0);
      dsTxN       : out   Slv7Array(1 downto 0);
      -- AMC's LS ports
      frqTbl      : inout slv      (1 downto 0);
      frqSel      : inout Slv4Array(1 downto 0);
      bwSel       : inout Slv2Array(1 downto 0);
      inc         : out   slv      (1 downto 0);
      dec         : out   slv      (1 downto 0);
      sfOut       : inout Slv2Array(1 downto 0);
      rate        : inout Slv2Array(1 downto 0);
      bypass      : out   slv      (1 downto 0);
      pllRst      : out   slv      (1 downto 0);
      lol         : in    slv      (1 downto 0);
      los         : in    slv      (1 downto 0);
      hsrScl      : inout Slv3Array(1 downto 0);
      hsrSda      : inout Slv3Array(1 downto 0);
      amcScl      : inout slv      (1 downto 0);
      amcSda      : inout slv      (1 downto 0);
      amcRstN     : out   slv      (1 downto 0);
      -- RTM LS ports
      --rtmLsP      : out   slv(35 downto 32);
      --rtmLsN      : out   slv(35 downto 32);
      ----------------
      -- Core Ports --
      ----------------   
      -- Common Fabric Clock
      fabClkP         : in    sl;
      fabClkN         : in    sl;
      -- XAUI Ports
      ethRxP          : in    slv(3 downto 0);
      ethRxN          : in    slv(3 downto 0);
      ethTxP          : out   slv(3 downto 0);
      ethTxN          : out   slv(3 downto 0);
      ethClkP         : in    sl;
      ethClkN         : in    sl;
       -- Backplane MPS Ports
      bpClkIn         : in    sl;
      bpClkOut        : out   sl;
      bpBusRxP        : in    slv(14 downto 1);
      bpBusRxN        : in    slv(14 downto 1);
      -- LCLS Timing Ports
      timingTxP        : out   sl; -- Synchronous Timing Distribution (zone 2)
      timingTxN        : out   sl;
      timingRxP        : in    sl; -- LCLS-I Timing Input
      timingRxN        : in    sl;
      timingRefClkInP  : in    sl;
      timingRefClkInN  : in    sl;
      -- Upstream timing reception, feedback transmission (if slave)
      usRxP            : in    sl;
      usRxN            : in    sl;
      usTxP            : out   sl;
      usTxN            : out   sl;
      usClkP           : in    sl;  -- use genRef(0) with 371MHz osc
      usClkN           : in    sl;
      --
      timingRecClkOutP : out   sl;
      timingRecClkOutN : out   sl;
      timingClkSel     : out   sl;
      timingClkScl     : inout sl;  -- jitter cleaner (unused)
      timingClkSda     : inout sl;
      fpgaclk_P        : out   slv(3 downto 0);
      fpgaclk_N        : out   slv(3 downto 0);
      --fpgaclk0_P       : out   sl;
      --fpgaclk0_N       : out   sl;
      --fpgaclk2_P       : out   sl;
      --fpgaclk2_N       : out   sl;
      -- Crossbar Ports
      xBarSin          : out   slv(1 downto 0);
      xBarSout         : out   slv(1 downto 0);
      xBarConfig       : out   sl;
      xBarLoad         : out   sl;
      -- IPMC Ports
      ipmcScl          : inout sl;
      ipmcSda          : inout sl;
      -- Configuration PROM Ports
      calScl           : inout sl;
      calSda           : inout sl;
      -- DDR3L SO-DIMM Ports
      ddrScl           : inout sl;
      ddrSda           : inout sl;
      -- SYSMON Ports
      vPIn             : in    sl;
      vNIn             : in    sl);
end xpm_noRTM;

architecture top_level of xpm_noRTM is

begin

  U_Base : entity l2si.XpmBase
   generic map (
      TPD_G         =>       TPD_G         ,
      BUILD_INFO_G  =>       BUILD_INFO_G  ,
      USE_RTM_G     =>       false         )
   port map (
      -----------------------
      -- Application Ports --
      -----------------------
      -- -- AMC's HS Ports
      dsClkP      =>       dsClkP      , 
      dsClkN      =>       dsClkN      , 
      dsRxP       =>       dsRxP       , 
      dsRxN       =>       dsRxN       , 
      dsTxP       =>       dsTxP       , 
      dsTxN       =>       dsTxN       , 
      frqTbl      =>       frqTbl      , 
      frqSel      =>       frqSel      , 
      bwSel       =>       bwSel       , 
      inc         =>       inc         , 
      dec         =>       dec         , 
      sfOut       =>       sfOut       , 
      rate        =>       rate        , 
      bypass      =>       bypass      , 
      pllRst      =>       pllRst      , 
      lol         =>       lol         , 
      los         =>       los         , 
      hsrScl      =>       hsrScl      , 
      hsrSda      =>       hsrSda      , 
      amcScl      =>       amcScl      , 
      amcSda      =>       amcSda      ,
      amcRstN     =>       amcRstN     ,
      -- RTM LS ports
      --rtmLsP      =>       rtmLsP      , 
      --rtmLsN      =>       rtmLsN      , 
      ----------------
      -- Core Ports --
      ----------------   
      -- Common Fabric Clock
      fabClkP         =>       fabClkP         , 
      fabClkN         =>       fabClkN         , 
      -- XAUI Ports
      ethRxP          =>       ethRxP          , 
      ethRxN          =>       ethRxN          , 
      ethTxP          =>       ethTxP          , 
      ethTxN          =>       ethTxN          , 
      ethClkP         =>       ethClkP         , 
      ethClkN         =>       ethClkN         , 
       -- Backplane MPS Ports
      bpClkIn         =>       bpClkIn         , 
      bpClkOut        =>       bpClkOut        , 
      bpBusRxP        =>       bpBusRxP        , 
      bpBusRxN        =>       bpBusRxN        , 
      -- LCLS Timing Ports
      timingTxP        =>       timingTxP        , 
      timingTxN        =>       timingTxN        , 
      timingRxP        =>       timingRxP        , 
      timingRxN        =>       timingRxN        , 
      timingRefClkInP  =>       timingRefClkInP  , 
      timingRefClkInN  =>       timingRefClkInN  , 
      -- Upstream timing reception, feedback transmission (if slave)
      usRxP            =>       usRxP            , 
      usRxN            =>       usRxN            , 
      usTxP            =>       usTxP            , 
      usTxN            =>       usTxN            , 
      usClkP           =>       usClkP           , 
      usClkN           =>       usClkN           , 
      --
      timingRecClkOutP =>       timingRecClkOutP , 
      timingRecClkOutN =>       timingRecClkOutN , 
      timingClkSel     =>       timingClkSel     , 
      timingClkScl     =>       timingClkScl     , 
      timingClkSda     =>       timingClkSda     , 
      fpgaclk_P        =>       fpgaclk_P        , 
      fpgaclk_N        =>       fpgaclk_N        , 
      --fpgaclk0_P       =>       --fpgaclk0_P       , 
      --fpgaclk0_N       =>       --fpgaclk0_N       , 
      --fpgaclk2_P       =>       --fpgaclk2_P       , 
      --fpgaclk2_N       =>       --fpgaclk2_N       , 
      -- Crossbar Ports
      xBarSin          =>       xBarSin          , 
      xBarSout         =>       xBarSout         , 
      xBarConfig       =>       xBarConfig       , 
      xBarLoad         =>       xBarLoad         , 
      -- IPMC Ports
      ipmcScl          =>       ipmcScl          , 
      ipmcSda          =>       ipmcSda          , 
      -- Configuration PROM Ports
      calScl           =>       calScl           , 
      calSda           =>       calSda           , 
      -- DDR3L SO-DIMM Ports
      ddrScl           =>       ddrScl           , 
      ddrSda           =>       ddrSda           , 
      -- SYSMON Ports
      vPIn             =>       vPIn             , 
      vNIn             =>       vNIn             );

end top_level;
