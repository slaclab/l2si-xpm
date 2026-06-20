############################
# DO NOT EDIT THE CODE BELOW
############################

# Load RUCKUS environment and library
source -quiet $::env(RUCKUS_DIR)/vivado_proc.tcl

# Load submodules' code and constraints
loadRuckusTcl $::env(TOP_DIR)/submodules/axi-pcie-core/hardware/XilinxVariumC1100
loadRuckusTcl $::env(TOP_DIR)/submodules/axi-pcie-core/hardware/XilinxVariumC1100/pcie-4x8
loadRuckusTcl $::env(TOP_DIR)/submodules/surf
loadRuckusTcl $::env(TOP_DIR)/submodules/lcls-timing-core
loadRuckusTcl $::env(TOP_DIR)/submodules/l2si-core

loadRuckusTcl "$::DIR_PATH/../../common/xpm"
#loadRuckusTcl "$::DIR_PATH/../../common/gtyUltraScale+"

# Adding the Si5349 configuration
add_files -norecurse "$::DIR_PATH/../../common/pll-config/Si5394A_GT_REFCLK_186MHz.mem"

# Load target's source code and constraints
loadSource      -dir  "$::DIR_PATH/hdl/"
loadConstraints -dir  "$::DIR_PATH/hdl/"
