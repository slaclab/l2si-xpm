# Load RUCKUS library
source -quiet $::env(RUCKUS_DIR)/vivado_proc.tcl

# Load Source Code
loadSource -dir "$::DIR_PATH/coregen/"
#loadSource -lib l2si_core -dir "$::DIR_PATH/coregen/"
