# Load RUCKUS library
source -quiet $::env(RUCKUS_DIR)/vivado_proc.tcl

# Load Source Code
loadSource -lib l2si -dir "$::DIR_PATH/rtl/"

# Load Amc Common Carrier modules
if { $::env(PRJ_PART) == "XCKU040-FFVA1156-2-E" } {
   loadSource -lib l2si -dir "$::DIR_PATH/rtl/amcc"
}
