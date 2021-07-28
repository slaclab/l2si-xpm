# Load RUCKUS environment and library
source -quiet $::env(RUCKUS_DIR)/vivado_proc.tcl

# Load submodules' code and constraints
loadRuckusTcl $::env(TOP_DIR)/submodules

loadRuckusTcl $::env(TOP_DIR)/common/xpm
loadRuckusTcl $::env(TOP_DIR)/common/gthUltraScale

# Load target's source code and constraints
loadSource      -dir  "$::DIR_PATH/hdl/"
loadConstraints -dir  "$::DIR_PATH/hdl/"
#loadConstraints -dir  $::env(TOP_DIR)/common/xpm/xdc
