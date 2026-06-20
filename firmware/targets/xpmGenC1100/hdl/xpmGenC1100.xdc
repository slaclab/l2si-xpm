#set_property USER_SLR_ASSIGNMENT SLR1 [get_cells {U_HSIO}]
#set_property USER_SLR_ASSIGNMENT SLR0 [get_cells {U_HbmDmaBuffer}]
set_property USER_SLR_ASSIGNMENT SLR0 [get_cells {U_Core}]
#set_property USER_SLR_ASSIGNMENT SLR0 [get_cells {U_ExtendedCore}]

#create_clock -period 6.400 -name qsfp0RefClkP [get_ports {qsfp0RefClkP}] ;
#create_clock -period 6.400 -name qsfp1RefClkP [get_ports {qsfp1RefClkP}] ;
create_clock -period 5.384 -name qsfp0RefClkP [get_ports {qsfp0RefClkP}] ;
create_clock -period 5.384 -name qsfp1RefClkP [get_ports {qsfp1RefClkP}] ;

set_clock_groups -asynchronous \
   -group [get_clocks -include_generated_clocks {qsfp0RefClkP}] \
   -group [get_clocks -include_generated_clocks {qsfp1RefClkP}] \
   -group [get_clocks -include_generated_clocks {pciRefClk0}] \
   -group [get_clocks -include_generated_clocks {userClkP}]

set_clock_groups -asynchronous \
   -group [get_clocks -of_objects [get_pins U_Core/REAL_PCIE.U_AxiPciePhy/U_AxiPcie/inst/pcie4c_ip_i/inst/XilinxVariumC1100PciePhyGen4x8_pcie4c_ip_gt_top_i/diablo_gt.diablo_gt_phy_wrapper/phy_clk_i/bufg_gt_userclk/O]] \
   -group [get_clocks -of_objects [get_pins U_HSIO/U_axilClk/MmcmGen.U_Mmcm/CLKOUT0]] \
   -group [get_clocks -of_objects [get_pins U_HSIO/U_GTGCLK/MmcmGen.U_Mmcm/CLKOUT0]]


set_clock_groups -asynchronous \
   -group [get_clocks -of_objects [get_pins {U_Core/REAL_PCIE.U_AxiPciePhy/U_AxiPcie/inst/pcie4c_ip_i/inst/XilinxVariumC1100PciePhyGen4x8_pcie4c_ip_gt_top_i/diablo_gt.diablo_gt_phy_wrapper/phy_clk_i/bufg_gt_userclk/O}]] \
   -group [get_clocks {qsfp0RefClkP}] \
   -group [get_clocks -of_objects [get_pins {U_HSIO/U_axilClk/PllGen.U_Pll/CLKOUT0}]]

#set_clock_groups -asynchronous \
#   -group [get_clocks -of_objects [get_pins U_HSIO/U_GTGCLK/MmcmGen.U_Mmcm/CLKOUT0]] \
#   -group [get_clocks -of_objects [get_pins -hier -filter {NAME =~ U_HSIO/GEN_XPMASYNC.U_XpmAsync/TimingGtCoreWrapper_1/*/RXOUTCLK}]] \
#   -group [get_clocks -of_objects [get_pins -hier -filter {NAME =~ U_HSIO/GEN_XPMASYNC.U_XpmAsync/TimingGtCoreWrapper_1/*/TXOUTCLKPCS}]] \
#   -group [get_clocks -of_objects [get_pins -hier -filter {NAME =~ U_HSIO/GEN_AMC_MGT[?].U_Rcvr/GEN_CTRL[?].GEN_GTY.U_TimingGtyCore/*/RXOUTCLK}]] \
#   -group [get_clocks -of_objects [get_pins -hier -filter {NAME =~ U_HSIO/GEN_AMC_MGT[?].U_Rcvr/GEN_CTRL[?].GEN_GTY.U_TimingGtyCore/*/TXOUTCLK}]] \
#   -group [get_clocks -of_objects [get_pins -hier -filter {NAME =~ U_HSIO/GEN_AMC_MGT[?].U_Rcvr/GEN_CTRL[?].GEN_GTY.U_TimingGtyCore/*/TXOUTCLKPCS}]]
#   -group [get_clocks -include_generated_clocks {qsfp0RefClkP}] \
   
set_clock_groups -asynchronous \
  -group [get_clocks -of_objects [get_pins -hier -filter {NAME =~ U_HSIO/GEN_AMC_MGT[?].GEN_AMC.U_Rcvr/*/RXOUTCLK}]] \
  -group [get_clocks -of_objects [get_pins -hier -filter {NAME =~ U_HSIO/GEN_AMC_MGT[?].GEN_AMC.U_Rcvr/*/TXOUTCLK}]] \
  -group [get_clocks -of_objects [get_pins -hier -filter {NAME =~ U_HSIO/GEN_AMC_MGT[?].GEN_AMC.U_Rcvr/*/TXOUTCLKPCS}]] \
  -group [get_clocks -of_objects [get_pins U_HSIO/U_axilClk/MmcmGen.U_Mmcm/CLKOUT0]] \
  -group [get_clocks -of_objects [get_pins U_HSIO/U_axilClk/MmcmGen.U_Mmcm/CLKOUT1]] \
  -group [get_clocks -include_generated_clocks -of_objects [get_pins U_HSIO/U_GTGCLK/*/CLKOUT0]]

