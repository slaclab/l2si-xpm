-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Wrapper for GTH Core
-------------------------------------------------------------------------------
-- This file is part of 'LCLS Timing Core'.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'LCLS Timing Core', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;


library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;

library unisim;
use unisim.vcomponents.all;

entity TimingGtCoreWrapper is
   generic (
      TPD_G             : time             := 1 ns;
      AXI_CLK_FREQ_G    : real             := 104.167e6;
      AXIL_BASE_ADDR_G  : slv(31 downto 0) );
   port (
      -- AXI-Lite Port
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType;
      -- StableClk (which is GT's drpClk) in the IP core configured for 156.25MHz/2 (78.125MHz)
      stableClk       : in  sl;  -- Unused in GTHE3, but used in GTHE4/GTYE4
      stableRst       : in  sl;  -- Unused in GTHE3, but used in GTHE4/GTYE4
      -- GTH FPGA IO
      gtRefClk        : in  sl;  -- or gtgRefClk
      gtRefClkDiv2    : in  sl;  -- Unused in GTHE3, but used in GTHE4/GTYE4
      gtRxP           : in  sl;
      gtRxN           : in  sl;
      gtTxP           : out sl;
      gtTxN           : out sl;

      -- Rx ports
      rxControl      : in  TimingPhyControlType;
      rxStatus       : out TimingPhyStatusType;
      rxUsrClkActive : in  sl;
      rxCdrStable    : out sl;
      rxUsrClk       : in  sl;
      rxData         : out slv(15 downto 0);
      rxDataK        : out slv(1 downto 0);
      rxDispErr      : out slv(1 downto 0);
      rxDecErr       : out slv(1 downto 0);
      rxOutClk       : out sl;

      -- Tx Ports
      txControl      : in  TimingPhyControlType;
      txStatus       : out TimingPhyStatusType;
      txUsrClk       : in  sl;
      txUsrClkActive : in  sl;
      txData         : in  slv(15 downto 0);
      txDataK        : in  slv(1 downto 0);
      txOutClk       : out sl;

      loopback       : in slv(2 downto 0));
end entity TimingGtCoreWrapper;

architecture rtl of TimingGtCoreWrapper is

begin

  U_Core : entity lcls_timing_core.TimingGtCoreWrapper
    generic map (TPD_G             => TPD_G,
                 AXI_CLK_FREQ_G    => AXI_CLK_FREQ_G,
                 AXIL_BASE_ADDR_G  => AXIL_BASE_ADDR_G,
                 GTY_DRP_OFFSET_G  => x"00004000",
                 LCLS1_ONLY_G      => false,
                 EXTREF_G          => false)
    port map (
      axilClk         => axilClk,
      axilRst         => axilRst,
      axilReadMaster  => axilReadMaster,
      axilReadSlave   => axilReadSlave,
      axilWriteMaster => axilWriteMaster,
      axilWriteSlave  => axilWriteSlave,
      stableClk       => stableClk,
      stableRst       => stableRst,
      gtRefClk        => '0',
      gtRefClkDiv2    => gtRefClkDiv2, -- 186 MHz
      gtgRefClk       => gtRefClk,     -- 371 MHz
      cpllRefClkSel   => "111",
      gtRxP           => gtRxP,
      gtRxN           => gtRxN,
      gtTxP           => gtTxP,
      gtTxN           => gtTxN,
      rxControl       => rxControl,
      rxStatus        => rxStatus,
      rxUsrClkActive  => rxUsrClkActive,
      rxCdrStable     => rxCdrStable,
      rxUsrClk        => rxUsrClk,
      rxData          => rxData,
      rxDataK         => rxDataK,
      rxDispErr       => rxDispErr,
      rxDecErr        => rxDecErr,
      rxOutClk        => rxOutClk,
      txControl       => txControl,
      txStatus        => txStatus,
      txUsrClk        => txUsrClk,
      txUsrClkActive  => txUsrClkActive,
      txData          => txData,
      txDataK         => txDataK,
      txOutClk        => txOutClk,
      loopback        => loopback );
  
end architecture rtl;
