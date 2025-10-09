# Load RUCKUS library
source -quiet $::env(RUCKUS_DIR)/vivado_proc.tcl

# Load Source Code
loadSource -lib l2si -dir "$::DIR_PATH/rtl/"

# Load Amc Common Carrier modules
if { $::env(PRJ_PART) == "XCKU040-FFVA1156-2-E" } {
    loadSource -lib l2si -dir "$::DIR_PATH/rtl/amcc"
} else {
    if { $::env(PRJ_PART) == "xcku115-folvb2104-2-e" } {
	loadSource -lib l2si -dir "$::DIR_PATH/rtl/kcu1500"
	loadIpCore -path "$::DIR_PATH/coregen/axi_debug_bridge.xci"
    } else {
	loadSource -lib l2si -dir "$::DIR_PATH/rtl/c1100"
#    loadIpCore -path "$::DIR_PATH/coregen/axi_debug_bridge.xci"
    }
}
