##############################################################################
## This file is part of 'lcls2-epix-hr-pcie'.
## It is subject to the license terms in the LICENSE.txt file found in the
## top-level directory of this distribution and at:
##    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
## No part of 'lcls2-epix-hr-pcie', including this file,
## may be copied, modified, propagated, or distributed except according to
## the terms contained in the LICENSE.txt file.
##############################################################################

#  These are programmable clocks that will be set to 185.7 MHz
create_clock -period 5.385 -name qsfp0RefClkP0 [get_ports {qsfp0RefClkP[0]}]
create_clock -period 5.385 -name qsfp1RefClkP0 [get_ports {qsfp1RefClkP[0]}]
create_clock -period 6.400 -name qsfp0RefClkP1 [get_ports {qsfp0RefClkP[1]}]
create_clock -period 6.400 -name qsfp1RefClkP1 [get_ports {qsfp1RefClkP[1]}]

create_generated_clock -name pcieClk [get_pins {U_Core/REAL_PCIE.U_AxiPciePhy/U_AxiPcie/inst/pcie3_ip_i/inst/gt_top_i/gt_wizard.gtwizard_top_i/XilinxKcu1500PciePhy_pcie3_ip_gt_i/inst/gen_gtwizard_gthe3_top.XilinxKcu1500PciePhy_pcie3_ip_gt_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[25].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[3].GTHE3_CHANNEL_PRIM_INST/TXOUTCLK}]

# set_property USER_SLR_ASSIGNMENT SLR0 [get_cells {U_HSIO}]
# set_property USER_SLR_ASSIGNMENT SLR1 [get_cells {U_Application}]

set_property CLOCK_DEDICATED_ROUTE BACKBONE [get_nets U_axilClk/clkIn]

set_clock_groups -asynchronous \
		 -group [get_clocks [list [get_clocks -of_objects [get_pins {U_HSIO/GEN_AMC_MGT[0].U_Rcvr/GEN_CTRL[0].U_GthCore/inst/gen_gtwizard_gthe3_top.gt_dslink_ss_nophase_amc0_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[1].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[0].GTHE3_CHANNEL_PRIM_INST/RXOUTCLK}]] [get_clocks -of_objects [get_pins {U_HSIO/GEN_AMC_MGT[0].U_Rcvr/GEN_CTRL[1].U_GthCore/inst/gen_gtwizard_gthe3_top.gt_dslink_ss_nophase_amc0_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[1].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[0].GTHE3_CHANNEL_PRIM_INST/RXOUTCLK}]] [get_clocks -of_objects [get_pins {U_HSIO/GEN_AMC_MGT[0].U_Rcvr/GEN_CTRL[2].U_GthCore/inst/gen_gtwizard_gthe3_top.gt_dslink_ss_nophase_amc0_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[1].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[0].GTHE3_CHANNEL_PRIM_INST/RXOUTCLK}]] [get_clocks -of_objects [get_pins {U_HSIO/GEN_AMC_MGT[0].U_Rcvr/GEN_CTRL[3].U_GthCore/inst/gen_gtwizard_gthe3_top.gt_dslink_ss_nophase_amc0_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[1].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[0].GTHE3_CHANNEL_PRIM_INST/RXOUTCLK}]] [get_clocks -of_objects [get_pins {U_HSIO/GEN_AMC_MGT[1].U_Rcvr/GEN_CTRL[0].U_GthCore/inst/gen_gtwizard_gthe3_top.gt_dslink_ss_nophase_amc0_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[1].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[0].GTHE3_CHANNEL_PRIM_INST/RXOUTCLK}]] [get_clocks -of_objects [get_pins {U_HSIO/GEN_AMC_MGT[1].U_Rcvr/GEN_CTRL[1].U_GthCore/inst/gen_gtwizard_gthe3_top.gt_dslink_ss_nophase_amc0_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[1].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[0].GTHE3_CHANNEL_PRIM_INST/RXOUTCLK}]] [get_clocks -of_objects [get_pins {U_HSIO/GEN_AMC_MGT[1].U_Rcvr/GEN_CTRL[2].U_GthCore/inst/gen_gtwizard_gthe3_top.gt_dslink_ss_nophase_amc0_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[1].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[0].GTHE3_CHANNEL_PRIM_INST/RXOUTCLK}]] [get_clocks -of_objects [get_pins {U_HSIO/GEN_AMC_MGT[1].U_Rcvr/GEN_CTRL[3].U_GthCore/inst/gen_gtwizard_gthe3_top.gt_dslink_ss_nophase_amc0_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[1].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[0].GTHE3_CHANNEL_PRIM_INST/RXOUTCLK}]]]] \
		 -group [get_clocks qsfp0RefClkP0] \
		 -group [get_clocks qsfp0RefClkP1] \
		 -group [get_clocks qsfp1RefClkP0] \
		 -group [get_clocks -of_objects [get_pins U_HSIO/U_axilClk/PllGen.U_Pll/CLKOUT0]]


set_clock_groups -asynchronous \
		 -group [get_clocks -include_generated_clocks qsfp0RefClkP0] \
		 -group [get_clocks -include_generated_clocks qsfp0RefClkP1] \
		 -group [get_clocks -include_generated_clocks qsfp1RefClkP0] \
		 -group [get_clocks -include_generated_clocks qsfp1RefClkP1] \
		 -group [get_clocks -include_generated_clocks pciRefClkP]

set_clock_groups -asynchronous \
		 -group [get_clocks qsfp0RefClkP0] \
		 -group [get_clocks -of_objects [get_pins {U_HSIO/GEN_XPMASYNC.U_XpmAsync/TimingGtCoreWrapper_1/GEN_EXTREF.U_TimingGthCore/inst/gen_gtwizard_gthe3_top.TimingGth_extref_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[0].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[0].GTHE3_CHANNEL_PRIM_INST/TXOUTCLK}]] \
		 -group [get_clocks -of_objects [get_pins {U_HSIO/GEN_XPMASYNC.U_XpmAsync/TimingGtCoreWrapper_1/GEN_EXTREF.U_TimingGthCore/inst/gen_gtwizard_gthe3_top.TimingGth_extref_gtwizard_gthe3_inst/gen_gtwizard_gthe3.gen_channel_container[0].gen_enabled_channel.gthe3_channel_wrapper_inst/channel_inst/gthe3_channel_gen.gen_gthe3_channel_inst[0].GTHE3_CHANNEL_PRIM_INST/RXOUTCLK}]]
