############################
# DO NOT EDIT THE CODE BELOW
############################

# Load RUCKUS environment and library
source -quiet $::env(RUCKUS_DIR)/vivado_proc.tcl

# Load submodules' code and constraints
loadRuckusTcl $::env(TOP_DIR)/submodules

loadRuckusTcl "$::DIR_PATH/../../common/xpm"
loadRuckusTcl "$::DIR_PATH/../../common/gthUltraScale"

# Load target's source code and constraints
loadSource      -dir  "$::DIR_PATH/hdl/"
loadConstraints -dir  "$::DIR_PATH/../../common/xpm/xdc/"
loadConstraints -dir  "$::DIR_PATH/hdl/"
