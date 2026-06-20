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

entity xpmC1100 is
   generic (
      TPD_G          : time    := 1 ns;
      ROGUE_SIM_EN_G : boolean := false;
      BUILD_INFO_G   : BuildInfoType);
   port (
      ---------------------
      --  Application Ports
      ---------------------
      -- QSFP[0] Ports
      qsfp0RefClkP : in    sl;
      qsfp0RefClkN : in    sl;
      qsfp0RxP     : in    slv(3 downto 0);
      qsfp0RxN     : in    slv(3 downto 0);
      qsfp0TxP     : out   slv(3 downto 0);
      qsfp0TxN     : out   slv(3 downto 0);
      -- QSFP[1] Ports
      qsfp1RefClkP : in    sl;
      qsfp1RefClkN : in    sl;
      qsfp1RxP     : in    slv(3 downto 0);
      qsfp1RxN     : in    slv(3 downto 0);
      qsfp1TxP     : out   slv(3 downto 0);
      qsfp1TxN     : out   slv(3 downto 0);
      --------------
      --  Core Ports
      --------------
      -- System Ports
      userClkP     : in    sl;
      userClkN     : in    sl;
      hbmRefClkP   : in    sl;
      hbmRefClkN   : in    sl;
      -- Card Management Solution (CMS) Interface
      -- cmsHbmCatTrip: in    sl;
      -- cmsHbmTemp   : in    Slv7Array(1 downto 0);
      cmsUartRxd   : in    sl;
      cmsUartTxd   : out   sl;
      cmsGpio      : in    slv(3 downto 0);
      -- HBM Ports
      hbmCatTrip   : out   sl;
      -- SI5394 Ports
      si5394Scl    : inout sl;
      si5394Sda    : inout sl;
      si5394IrqL   : in    sl;
      si5394LolL   : in    sl;
      si5394LosL   : in    sl;
      si5394RstL   : out   sl;
      -- PCIe Ports
      pciRstL      : in    sl;
      pciRefClkP   : in    slv(1 downto 0);
      pciRefClkN   : in    slv(1 downto 0);
      pciRxP       : in    slv(7 downto 0);
      pciRxN       : in    slv(7 downto 0);
      pciTxP       : out   slv(7 downto 0);
      pciTxN       : out   slv(7 downto 0));
end xpmC1100;

architecture top_level of xpmC1100 is

   constant DMA_AXIS_CONFIG_C : AxiStreamConfigType := ssiAxiStreamConfig(8, TKEEP_COMP_C, TUSER_FIRST_LAST_C, 8, 2);  -- 64-bit interface
   constant DMA_SIZE_C        : positive            := 2;

   signal userClk          : sl;
   
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

   --  Don't know which pins connect these signals
   signal cmsHbmCatTrip : sl := '0';
   signal cmsHbmTemp    : Slv7Array(1 downto 0) := (others=>"0000000");
   
begin

   -----------------------
   -- AXI-PCIE-CORE Module
   -----------------------
   U_Core : entity axi_pcie_core.XilinxVariumC1100Core
      generic map (
         TPD_G                => TPD_G,
         QSFP_CDR_DISABLE_G   => true,
         SI5394_INIT_FILE_G   => "Si5394A_GT_REFCLK_186MHz.mem",
--         SI5394_INIT_FILE_G   => "Si5394A_GT_REFCLK_156_25MHz.mem",
--         SI5394_INIT_FILE_G   => "C1100_185_714MHz-Register.mem",
         ROGUE_SIM_EN_G       => ROGUE_SIM_EN_G,
         ROGUE_SIM_CH_COUNT_G => 4,     -- 4 Virtual Channels per DMA lane
         DRIVER_TYPE_ID_G     => toSlv(0,32),
         BUILD_INFO_G         => BUILD_INFO_G,
         DMA_AXIS_CONFIG_G    => DMA_AXIS_CONFIG_C,
         DATAGPU_EN_G         => false,
         DMA_SIZE_G           => DMA_SIZE_C)
      port map (
         ------------------------
         --  Top Level Interfaces
         ------------------------
         userClk        => userClk,
         hbmRefClk      => open,
         -- DMA Interfaces
         dmaClk         => dmaClk,
         dmaRst         => dmaRst,
         dmaObMasters   => dmaObMasters,
         dmaObSlaves    => dmaObSlaves,
         dmaIbMasters   => dmaIbMasters,
         dmaIbSlaves    => dmaIbSlaves,
         -- User General Purpose AXI4 Interfaces
         -- usrReadMaster                   => usrReadMasters (0),
         -- usrReadSlave                    => usrReadSlaves (0),
         -- usrWriteMaster                  => usrWriteMasters (0),
         -- usrWriteSlave                   => usrWriteSlaves (0),
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
         -- Card Management Solution (CMS) Interface
         cmsHbmCatTrip  => cmsHbmCatTrip,
         cmsHbmTemp     => cmsHbmTemp,
         cmsUartRxd     => cmsUartRxd,
         cmsUartTxd     => cmsUartTxd,
         cmsGpio        => cmsGpio,
         -- System Ports
         userClkP       => userClkP,
         userClkN       => userClkN,
         hbmRefClkP     => hbmRefClkP,
         hbmRefClkN     => hbmRefClkN,
         -- SI5394 Ports
         si5394Scl      => si5394Scl,
         si5394Sda      => si5394Sda,
         si5394IrqL     => si5394IrqL,
         si5394LolL     => si5394LolL,
         si5394LosL     => si5394LosL,
         si5394RstL     => si5394RstL,
         -- PCIe Ports
         pciRstL        => pciRstL,
         pciRefClkP(0)  => pciRefClkP(0),
         pciRefClkN(0)  => pciRefClkN(0),
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
        XPM_MODE_G          => "XpmAsync",
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
         userClk               => userClk,
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
