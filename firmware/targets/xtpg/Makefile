export OVERRIDE_SUBMODULE_LOCKS = 0
export GIT_BYPASS = 1
##########################################################################################
# Applications must define the 32-bitFirmware Version Number
# PRJ_VERSION = 0x00000000
# PRJ_VERSION = 0x00000001 - Use MMCM phase shift to narrow arbitrary phase upon reset
# PRJ_VERSION = 0x00010001 - Hardwire UsRxEnable=1
# PRJ_VERSION = 0x00000002 - Make usRxEnable programmable.  Fix last MMCM phase scan.
# PRJ_VERSION = 0x00000003 - Push stats via UDP.
# PRJ_VERSION = 0x00000004 - Keep messages synchronous across groups.
# PRJ_VERSION = 0x00000005 - Connect recTimingRst to txLink txreset,txbypassrst
# PRJ_VERSION = 0x00000006 - Allow messages to respect inhibit and remain queued
##########################################################################################
#export PRJ_VERSION = 0x00000006

##########################################################################################
# Applications must define if using advance AMC carrier build
# 
# AMC_ADV_BUILD = 0: Uses pre-built AmcCarrierCore.vhd image (99% case)
# AMC_ADV_BUILD = 1: Allows full configuration of AmcCarrierCore generics
#
# Here is a possible list of reason why you would need AMC_ADV_BUILD = 1
#     1) Timing Generator application
#     2) Not using BSA engine and would like to free up resources
#     3) Using zone3 (RTM) instead of zone2 (backplane) for Ethernet
#
# Note: (AMC_ADV_BUILD = 1) requires a munch longer build time
##########################################################################################
# AMC_ADV_BUILD is defined by application
export AMC_ADV_BUILD = 1
export TIMING_EXT_PKG = 1
export USE_XVC_DEBUG = 1
#export USE_XVC_DEBUG = 0

##########################################################################################
# Applications must define if using zone3 (RTM) ETH (instead of zone2 ETH)
# Note: AMC_ADV_BUILD = 0 does NOT support zone3 (RTM) ETH
##########################################################################################

# RTM_ETH is defined by application
export RTM_ETH = 0

##########################################################################################
# Applications must define the application specific hardware that's being 
# used in AppCore.vhd
#
# Note:  In general for RTM_INTF,
#        If carrier is version C05 (or ealier), use RTM_INTF = "Version1"  
#        If carrier is version C06 (or later),  use RTM_INTF = "Version2"
##########################################################################################

# Controls inclusion of constraints
# AMC_TYPE_BAY0 & AMC_INTF_BAY0 are defined by application
export AMC_TYPE_BAY0 = AmcMpsSfp
export AMC_INTF_BAY0 = Version2

# AMC_TYPE_BAY1 & AMC_INTF_BAY1 are defined by application
export AMC_TYPE_BAY1 = AmcMpsSfp
export AMC_INTF_BAY1 = Version2

# RTM_TYPE & RTM_INTF are defined by application
export RTM_TYPE = RtmEmpty
export RTM_INTF = Version1

##########################################################################################
# Applications must define the application common path where AppCore.vhd exists
##########################################################################################

# COMMON_FILE is defined by application
export COMMON_FILE = xpm

##########################################################################################
# Application have the option to remove unused source code from Vivado project
#     If REMOVE_UNUSED_CODE = 0, all source code is kept after importing to Vivado project
#     If REMOVE_UNUSED_CODE = 1, all unused source code is removed from Vivado project
##########################################################################################
export REMOVE_UNUSED_CODE = 1

##########################################################################################
############################  DO NOT EDIT THE CODE BELOW  ################################
##########################################################################################

# Define target output
target: prom

# Define target part
export PRJ_PART = XCKU040-FFVA1156-2-E
export BOARD_PART = 
export PRJ_FMC =

# Use top level makefile
include ../shared_config.mk
include ../../submodules/ruckus/system_vivado.mk
