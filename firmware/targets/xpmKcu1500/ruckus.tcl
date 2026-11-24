############################
# DO NOT EDIT THE CODE BELOW
############################

# Load RUCKUS environment and library
source -quiet $::env(RUCKUS_DIR)/vivado_proc.tcl

# Check for Vivado version 2026.1 (or later)
# Needed to prevent errors from IP core upgrade of lcls-timing-core/timing_extref.xci
if { [VersionCheck 2025.1 ] < 0 } {
   exit -1
}

# Load submodules' code and constraints
loadRuckusTcl $::env(TOP_DIR)/submodules/axi-pcie-core/hardware/XilinxKcu1500
loadRuckusTcl $::env(TOP_DIR)/submodules/surf
loadRuckusTcl $::env(TOP_DIR)/submodules/lcls-timing-core
loadRuckusTcl $::env(TOP_DIR)/submodules/l2si-core

loadRuckusTcl "$::DIR_PATH/../../common/xpm"
loadRuckusTcl "$::DIR_PATH/../../common/gthUltraScale"

# Load target's source code and constraints
loadSource      -dir  "$::DIR_PATH/hdl/"
loadConstraints -dir  "$::DIR_PATH/hdl/"
