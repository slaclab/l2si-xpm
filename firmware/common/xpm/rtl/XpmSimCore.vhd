-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmGenKcu1500.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2026-01-16
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--
-- This module will generate L2S-I timing (XPM) for use in teststands.
--
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
use surf.AxiLitePkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;
use lcls_timing_core.TPGPkg.all;

library l2si_core;
use l2si_core.XpmPkg.all;

library l2si;

entity XpmSimCore is
   generic (
      TPD_G               : time    := 1 ns );
   port (
     signal axilClk               : in  sl;
     signal axilRst               : in  sl;
     signal axilReadMaster        : in  AxiLiteReadMasterType;
     signal axilReadSlave         : out AxiLiteReadSlaveType;
     signal axilWriteMaster       : in  AxiLiteWriteMasterType;
     signal axilWriteSlave        : out AxiLiteWriteSlaveType;

     signal timingPhyClk          : in  sl;
     signal timingPhyRst          : in  sl;
     signal recStream             : out XpmStreamType );
end XpmSimCore;

architecture rtl of XpmSimCore is

   signal tpgConfig : TPGConfigType;
   signal tpgStatus : TPGStatusType;
   
   -- Timing Interface (timingClk domain)
   signal txStreams   : TimingSerialArray(2 downto 0);
   signal txStreamIds : Slv4Array        (2 downto 0);
   signal txAdvance   : slv              (2 downto 0) := (others=>'0');
   signal txFiducial  : sl;

begin

   TPGMiniReg_Inst : entity l2si.TPGMiniReg
      generic map (
         TPD_G       => TPD_G )
      port map (
         axiClk         => axilClk,
         axiRst         => axilRst,
         axiReadMaster  => axilReadMaster,
         axiReadSlave   => axilReadSlave,
         axiWriteMaster => axilWriteMaster,
         axiWriteSlave  => axilWriteSlave,
         --
         clk            => timingPhyClk,
         rst            => timingPhyRst,
         status         => tpgStatus,
         config         => tpgConfig,
         irqActive      => '0' );

   U_TPGMiniCore : entity l2si.TPGMiniClock
     generic map (
       TPD_G        => TPD_G,
       STREAM_INTF  => true,
       AC_PERIOD    => 2574 )
     port map (
       txClk           => timingPhyClk,
       txRst           => timingPhyRst,
       txRdy           => '1',
       config          => tpgConfig,
       status          => tpgStatus,
       -- alternate output (STREAM_INTF=true)
       streams         => txStreams  (0 downto 0),
       streamIds       => txStreamIds(0 downto 0),
       advance         => txAdvance  (0 downto 0),
       fiducial        => txFiducial );
   
   U_SimSerializer : entity lcls_timing_core.TimingSerializer
      generic map (
         STREAMS_C => 3)
      port map (
         clk       => timingPhyClk,
         rst       => timingPhyRst,
         fiducial  => txFiducial,
         streams   => txStreams,
         streamIds => txStreamIds,
         advance   => txAdvance,
         data      => open,
         dataK     => open);
   
   recStream.fiducial <= txFiducial;
   recStream.streams  <= txStreams;
   recStream.advance  <= txAdvance;
   
end rtl;

