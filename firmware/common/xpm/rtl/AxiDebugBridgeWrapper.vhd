-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : AxiDebugBridgeWrapper.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2025-0508
-- Last update: 2025-05-08
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Wrapper for Debug Bridge over AXI
-------------------------------------------------------------------------------
-- This file is part of 'LCLS2 XPM Core'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'LCLS2 XPM Core', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;


library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;

entity AxiDebugBridgeWrapper is
  port (
    axilClk         : in  sl;
    axilRst         : in  sl;
    axilReadMaster  : in  AxiLiteReadMasterType;
    axilReadSlave   : out AxiLiteReadSlaveType;
    axilWriteMaster : in  AxiLiteWriteMasterType;
    axilWriteSlave  : out AxiLiteWriteSlaveType );
end AxiDebugBridgeWrapper;

architecture rtl of AxiDebugBridgeWrapper is

  COMPONENT axi_debug_bridge
    PORT (
      s_axi_aclk : IN sl;
      s_axi_aresetn : IN sl;
      S_AXI_araddr : IN slv(4 DOWNTO 0);
      S_AXI_arprot : IN slv(2 DOWNTO 0);
      S_AXI_arready : OUT sl;
      S_AXI_arvalid : IN sl;
      S_AXI_awaddr : IN slv(4 DOWNTO 0);
      S_AXI_awprot : IN slv(2 DOWNTO 0);
      S_AXI_awready : OUT sl;
      S_AXI_awvalid : IN sl;
      S_AXI_bready : IN sl;
      S_AXI_bresp : OUT slv(1 DOWNTO 0);
      S_AXI_bvalid : OUT sl;
      S_AXI_rdata : OUT slv(31 DOWNTO 0);
      S_AXI_rready : IN sl;
      S_AXI_rresp : OUT slv(1 DOWNTO 0);
      S_AXI_rvalid : OUT sl;
      S_AXI_wdata : IN slv(31 DOWNTO 0);
      S_AXI_wready : OUT sl;
      S_AXI_wstrb : IN slv(3 DOWNTO 0);
      S_AXI_wvalid : IN sl 
      );
  END COMPONENT;

  signal naxilRst : sl;
  
begin

  naxilRst <= not axilRst;

  U_DebugBridge : axi_debug_bridge
    port map (
      s_axi_aclk    => axilClk,
      s_axi_aresetn => naxilRst,
      S_AXI_araddr  => axilReadMaster.araddr,
      S_AXI_arprot  => axilReadMaster.arprot,
      S_AXI_arready => axilReadSlave.arready,
      S_AXI_arvalid => axilReadMaster.arvalid,
      S_AXI_awaddr  => axilWriteMaster.awaddr,
      S_AXI_awprot  => axilWriteMaster.awprot,
      S_AXI_awready => axilWriteSlave.awready,
      S_AXI_awvalid => axilWriteMaster.awvalid,
      S_AXI_bready  => axilWriteMaster.bready,
      S_AXI_bresp   => axilWriteSlave.bresp,
      S_AXI_bvalid  => axilWriteSlave.bvalid,
      S_AXI_rdata   => axilReadSlave.rdata,
      S_AXI_rready  => axilReadMaster.rready,
      S_AXI_rresp   => axilReadSlave.rresp,
      S_AXI_rvalid  => axilReadSlave.rvalid,
      S_AXI_wdata   => axilWriteMaster.wdata,
      S_AXI_wready  => axilWriteSlave.wready,
      S_AXI_wstrb   => axilWriteMaster.wstrb,
      S_AXI_wvalid  => axilWriteMaster.wvalid
      );

end rtl;
