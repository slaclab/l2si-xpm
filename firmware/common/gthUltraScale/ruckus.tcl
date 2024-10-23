# Load RUCKUS library
source -quiet $::env(RUCKUS_DIR)/vivado_proc.tcl

if { $::env(PRJ_PART) == "XCKU040-FFVA1156-2-E" } {
    loadSource -path "$::DIR_PATH/coregen/gt_dslink_ss_nophase_amc0.dcp"
} else {
#    if { [VersionCheck 2021.1] > 0 } {
#	puts "Vivado version check failed for gs_dslink_ss_nophase_amc0.dcp"
#	exit -1
#    }
    loadSource -path "$::DIR_PATH/coregen/v2021.1/gt_dslink_ss_nophase_amc0.dcp"
}
