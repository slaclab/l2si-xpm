#export OVERRIDE_SUBMODULE_LOCKS = 1
#export GIT_BYPASS = 1
##########################################################################################
# Applications must define the 32-bitFirmware Version Number
# PRJ_VERSION = 0x00000013 - Adopt TimingExtnPkg and factorization
# PRJ_VERSION = 0x00000014 - Automate front panel link rx resets, fix paddr latching
# PRJ_VERSION = 0x00000015 - Add readout group registers and LCLS1 timing input
# PRJ_VERSION = 0x00000016 - Add cuRxEnable/usRxEnable
# PRJ_VERSION = 0x00000017 - Migrate to l2si-core.  Add link rxTimeOut.  Add link fulltime counts
# PRJ_VERSION = 0x00000018 - Add link groupMask.  Remove linkTxDelay, linkPartition.  
#                          - Change tmit PADDR to be configured via register access.
# PRJ_VERSION = 0x00000019 - Use obBsaMaster stream for sequence checkpoint notification
# PRJ_VERSION = 0x0000001A - Add parsing of partition messages for group link clear
# PRJ_VERSION = 0x0000001B - Fix transition/L1Accept coincidence
# PRJ_VERSION = 0x0000001C - Add AMC SFP I2C
# PRJ_VERSION = 0x0000001D - Changed XpmRxLink feedback state machine
# PRJ_VERSION = 0x0000001E - Forward and feedback link ID come from the same register
# PRJ_VERSION = 0x0000001F - Fix transition alignment across readout groups bug
# PRJ_VERSION = 0x00000020 - Add transitions which can be inhibited by deadtime and group step limits
# PRJ_VERSION = 0x00000021 - Add XpmMonitorStream
# PRJ_VERSION = 0x00000022 - Fix XpmSequence register access
# PRJ_VERSION = 0x00000023 - Fix multi-group transition timestamp.
#                            Add coupled group deadtime.
#                            Fix sequencer memory readback.
# PRJ_VERSION = 0x00000024 - Connect recTimingRst to txLink txreset,txbypassrst
# PRJ_VERSION = 0x00000025 - Allow messages to respect inhibit and remain queued
##########################################################################################
export PRJ_VERSION = 0x00000025

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
include ../../submodules/ruckus/system_vivado.mk
