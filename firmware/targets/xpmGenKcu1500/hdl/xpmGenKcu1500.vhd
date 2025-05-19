-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Camera link gateway PCIe card with PGPv2b
-------------------------------------------------------------------------------
-- This file is part of 'lcls2-epix-hr-pcie'.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'lcls2-epix-hr-pcie', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiPkg.all;
use surf.AxiLitePkg.all;
use surf.AxiStreamPkg.all;
use surf.SsiPkg.all;

library lcls2_pgp_fw_lib;
library l2si;
library axi_pcie_core;

library unisim;
use unisim.vcomponents.all;

entity xpmGenKcu1500 is
   generic (
      TPD_G          : time    := 1 ns;
      ROGUE_SIM_EN_G : boolean := false;
      PGP_TYPE_G     : string  := "PGP4";
      RATE_G         : string  := "6.25Gbps";
      BUILD_INFO_G   : BuildInfoType);
   port (
      ---------------------
      --  Application Ports
      ---------------------
      -- QSFP[0] Ports
      qsfp0RefClkP : in    slv(1 downto 0);
      qsfp0RefClkN : in    slv(1 downto 0);
      qsfp0RxP     : in    slv(3 downto 0);
      qsfp0RxN     : in    slv(3 downto 0);
      qsfp0TxP     : out   slv(3 downto 0);
      qsfp0TxN     : out   slv(3 downto 0);
      -- QSFP[1] Ports
      qsfp1RefClkP : in    slv(1 downto 0);
      qsfp1RefClkN : in    slv(1 downto 0);
      qsfp1RxP     : in    slv(3 downto 0);
      qsfp1RxN     : in    slv(3 downto 0);
      qsfp1TxP     : out   slv(3 downto 0);
      qsfp1TxN     : out   slv(3 downto 0);
      --------------
      --  Core Ports
      --------------
      -- System Ports
      emcClk       : in    sl;
      userClkP     : in    sl;
      userClkN     : in    sl;
      i2cRstL      : out   sl;
      i2cScl       : inout sl;
      i2cSda       : inout sl;
      -- QSFP[0] Ports
      qsfp0RstL    : out   sl;
      qsfp0LpMode  : out   sl;
      qsfp0ModSelL : out   sl;
      qsfp0ModPrsL : in    sl;
      -- QSFP[1] Ports
      qsfp1RstL    : out   sl;
      qsfp1LpMode  : out   sl;
      qsfp1ModSelL : out   sl;
      qsfp1ModPrsL : in    sl;
      -- Boot Memory Ports
      flashCsL     : out   sl;
      flashMosi    : out   sl;
      flashMiso    : in    sl;
      flashHoldL   : out   sl;
      flashWp      : out   sl;
      -- PCIe Ports
      pciRstL      : in    sl;
      pciRefClkP   : in    sl;
      pciRefClkN   : in    sl;
      pciRxP       : in    slv(7 downto 0);
      pciRxN       : in    slv(7 downto 0);
      pciTxP       : out   slv(7 downto 0);
      pciTxN       : out   slv(7 downto 0));
end xpmGenKcu1500;

architecture top_level of xpmGenKcu1500 is

   constant DMA_AXIS_CONFIG_C : AxiStreamConfigType := ssiAxiStreamConfig(8, TKEEP_COMP_C, TUSER_FIRST_LAST_C, 8, 2);  -- 64-bit interface
   constant AXIL_CLK_FREQ_C   : real                := 125.0E+6;  -- units of Hz
   constant DMA_SIZE_C        : positive            := 2;

   signal axilClk          : sl;
   signal axilRst          : sl;
   signal axilReadMaster   : AxiLiteReadMasterType;
   signal axilReadSlave    : AxiLiteReadSlaveType;
   signal axilWriteMaster  : AxiLiteWriteMasterType;
   signal axilWriteSlave   : AxiLiteWriteSlaveType;

   signal dmaClk       : sl;
   signal dmaRst       : sl;
   signal dmaObMasters : AxiStreamMasterArray(DMA_SIZE_C-1 downto 0) := (others => AXI_STREAM_MASTER_INIT_C);
   signal dmaObSlaves  : AxiStreamSlaveArray(DMA_SIZE_C-1 downto 0)  := (others => AXI_STREAM_SLAVE_FORCE_C);
   signal dmaIbMasters : AxiStreamMasterArray(DMA_SIZE_C-1 downto 0) := (others => AXI_STREAM_MASTER_INIT_C);
   signal dmaIbSlaves  : AxiStreamSlaveArray(DMA_SIZE_C-1 downto 0)  := (others => AXI_STREAM_SLAVE_FORCE_C);

begin

   -----------------------
   -- AXI-PCIE-CORE Module
   -----------------------
   U_Core : entity work.XilinxKcu1500Core
      generic map (
         TPD_G                => TPD_G,
         ROGUE_SIM_EN_G       => ROGUE_SIM_EN_G,
         ROGUE_SIM_CH_COUNT_G => 4,     -- 4 Virtual Channels per DMA lane
         BUILD_INFO_G         => BUILD_INFO_G,
         DMA_AXIS_CONFIG_G    => DMA_AXIS_CONFIG_C,
         DMA_SIZE_G           => DMA_SIZE_C)
      port map (
         ------------------------
         --  Top Level Interfaces
         ------------------------
         userClk156     => open,        -- from Si570
         -- DMA Interfaces
         dmaClk         => dmaClk,
         dmaRst         => dmaRst,
         dmaObMasters   => dmaObMasters,
         dmaObSlaves    => dmaObSlaves,
         dmaIbMasters   => dmaIbMasters,
         dmaIbSlaves    => dmaIbSlaves,
         -- AXI-Lite Interface
         appClk         => axilClk,
         appRst         => axilRst,
         appReadMaster  => axilReadMaster,
         appReadSlave   => axilReadSlave,
         appWriteMaster => axilWriteMaster,
         appWriteSlave  => axilWriteSlave,
         --------------
         --  Core Ports
         --------------
         -- System Ports
         emcClk         => emcClk,
         userClkP       => userClkP,
         userClkN       => userClkN,
         i2cRstL        => i2cRstL,
         i2cScl         => i2cScl,
         i2cSda         => i2cSda,
         -- QSFP[0] Ports
         qsfp0RstL      => qsfp0RstL,
         qsfp0LpMode    => qsfp0LpMode,
         qsfp0ModSelL   => qsfp0ModSelL,
         qsfp0ModPrsL   => qsfp0ModPrsL,
         -- QSFP[1] Ports
         qsfp1RstL      => qsfp1RstL,
         qsfp1LpMode    => qsfp1LpMode,
         qsfp1ModSelL   => qsfp1ModSelL,
         qsfp1ModPrsL   => qsfp1ModPrsL,
         -- Boot Memory Ports
         flashCsL       => flashCsL,
         flashMosi      => flashMosi,
         flashMiso      => flashMiso,
         flashHoldL     => flashHoldL,
         flashWp        => flashWp,
         -- PCIe Ports
         pciRstL        => pciRstL,
         pciRefClkP     => pciRefClkP,
         pciRefClkN     => pciRefClkN,
         pciRxP         => pciRxP,
         pciRxN         => pciRxN,
         pciTxP         => pciTxP,
         pciTxN         => pciTxN);

   ------------------
   -- Hardware Module
   ------------------
   U_HSIO : entity l2si.XpmBase
      generic map (
        TPD_G               => TPD_G,
        AXIL_BASE_G         => x"0080_0000",
        DMA_SIZE_G          => DMA_SIZE_C,
        DMA_AXIS_CONFIG_G   => DMA_AXIS_CONFIG_C )
      port map (
         -- AXI-Lite Interface (axilClk domain)
         axilClk               => axilClk,
         axilRst               => axilRst,
         axilReadMaster        => axilReadMaster ,
         axilReadSlave         => axilReadSlave  ,
         axilWriteMaster       => axilWriteMaster,
         axilWriteSlave        => axilWriteSlave ,
         dmaClk                => dmaClk,
         dmaRst                => dmaRst,
         ibDmaMasters          => dmaIbMasters,
         ibDmaSlaves           => dmaIbSlaves ,
         obDmaMasters          => dmaObMasters,
         obDmaSlaves           => dmaObSlaves ,
         ------------------
         --  Hardware Ports
         ------------------
         -- QSFP[0] Ports,
         qsfp0RefClkP          => qsfp0RefClkP,
         qsfp0RefClkN          => qsfp0RefClkN,
         qsfp0RxP              => qsfp0RxP,
         qsfp0RxN              => qsfp0RxN,
         qsfp0TxP              => qsfp0TxP,
         qsfp0TxN              => qsfp0TxN,
         -- QSFP[1] Ports
         qsfp1RefClkP          => qsfp1RefClkP,
         qsfp1RefClkN          => qsfp1RefClkN,
         qsfp1RxP              => qsfp1RxP,
         qsfp1RxN              => qsfp1RxN,
         qsfp1TxP              => qsfp1TxP,
         qsfp1TxN              => qsfp1TxN);

end top_level;
