##############################################################################
## This file is part of 'LCLS2 Common Carrier Core'.
## It is subject to the license terms in the LICENSE.txt file found in the
## top-level directory of this distribution and at:
##    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
## No part of 'LCLS2 Common Carrier Core', including this file,
## may be copied, modified, propagated, or distributed except according to
## the terms contained in the LICENSE.txt file.
##############################################################################

#######################
## Common Core Ports ##
#######################

# Common fabrication clock
set_property PACKAGE_PIN T5 [get_ports fabClkN]
set_property PACKAGE_PIN T6 [get_ports fabClkP]

# ETH Ports
set_property PACKAGE_PIN V5 [get_ports ethClkN]
set_property PACKAGE_PIN V6 [get_ports ethClkP]

# LCLS Timing Ports
set_property -dict {PACKAGE_PIN AH13 IOSTANDARD LVDS_25} [get_ports timingRecClkOutP]
set_property -dict {PACKAGE_PIN AJ13 IOSTANDARD LVDS_25} [get_ports timingRecClkOutN]
set_property -dict {PACKAGE_PIN K22 IOSTANDARD LVCMOS25} [get_ports timingClkSel]

set_property PACKAGE_PIN AJ3 [get_ports timingRxN]
set_property PACKAGE_PIN AJ4 [get_ports timingRxP]
set_property PACKAGE_PIN AK5 [get_ports timingTxN]
set_property PACKAGE_PIN AK6 [get_ports timingTxP]
set_property PACKAGE_PIN Y5 [get_ports timingRefClkInN]
set_property PACKAGE_PIN Y6 [get_ports timingRefClkInP]

# Secondary AMC Auxiliary Power Enable Port
#set_property -dict { PACKAGE_PIN H23 IOSTANDARD LVCMOS25 } [get_ports {enAuxPwrL}]

# DDR3L SO-DIMM Ports
set_property PACKAGE_PIN      G17         [get_ports {ddrClkP}]
set_property PACKAGE_PIN      G16         [get_ports {ddrClkN}]
set_property IOSTANDARD       DIFF_HSTL_I [get_ports {ddrClkP ddrClkN}]
set_property IBUF_LOW_PWR     FALSE       [get_ports {ddrClkP ddrClkN}]
set_property PULLTYPE         KEEPER      [get_ports {ddrClkP ddrClkN}]

set_property PACKAGE_PIN B10 [get_ports {ddrDqsP[0]}]
set_property PACKAGE_PIN A10 [get_ports {ddrDqsN[0]}]
set_property PACKAGE_PIN K10 [get_ports {ddrDqsP[1]}]
set_property PACKAGE_PIN J10 [get_ports {ddrDqsN[1]}]
set_property PACKAGE_PIN L13 [get_ports {ddrDqsP[2]}]
set_property PACKAGE_PIN K13 [get_ports {ddrDqsN[2]}]
set_property PACKAGE_PIN F13 [get_ports {ddrDqsP[3]}]
set_property PACKAGE_PIN E13 [get_ports {ddrDqsN[3]}]
set_property PACKAGE_PIN B29 [get_ports {ddrDqsP[4]}]
set_property PACKAGE_PIN A29 [get_ports {ddrDqsN[4]}]
set_property PACKAGE_PIN B24 [get_ports {ddrDqsP[5]}]
set_property PACKAGE_PIN A24 [get_ports {ddrDqsN[5]}]
set_property PACKAGE_PIN C21 [get_ports {ddrDqsP[6]}]
set_property PACKAGE_PIN C22 [get_ports {ddrDqsN[6]}]
set_property PACKAGE_PIN G20 [get_ports {ddrDqsP[7]}]
set_property PACKAGE_PIN F20 [get_ports {ddrDqsN[7]}]
set_property IOSTANDARD DIFF_SSTL15_DCI   [get_ports {ddrDqsP[*] ddrDqsN[*]}]
set_property OUTPUT_IMPEDANCE RDRV_40_40  [get_ports {ddrDqsP[*] ddrDqsN[*]}]
set_property SLEW          FAST           [get_ports {ddrDqsP[*] ddrDqsN[*]}]
set_property IBUF_LOW_PWR  FALSE          [get_ports {ddrDqsP[*] ddrDqsN[*]}]
set_property ODT           RTT_40         [get_ports {ddrDqsP[*] ddrDqsN[*]}]

set_property PACKAGE_PIN F8  [get_ports {ddrDm[0]}]
set_property PACKAGE_PIN L8  [get_ports {ddrDm[1]}]
set_property PACKAGE_PIN H11 [get_ports {ddrDm[2]}]
set_property PACKAGE_PIN E11 [get_ports {ddrDm[3]}]
set_property PACKAGE_PIN F27 [get_ports {ddrDm[4]}]
set_property PACKAGE_PIN E26 [get_ports {ddrDm[5]}]
set_property PACKAGE_PIN D23 [get_ports {ddrDm[6]}]
set_property PACKAGE_PIN G24 [get_ports {ddrDm[7]}]
set_property IOSTANDARD SSTL15_DCI        [get_ports {ddrDm[*]}]
set_property OUTPUT_IMPEDANCE RDRV_40_40  [get_ports {ddrDm[*]}]
set_property SLEW FAST                    [get_ports {ddrDm[*]}]

set_property PACKAGE_PIN B9  [get_ports {ddrDq[0]}]
set_property PACKAGE_PIN A9  [get_ports {ddrDq[1]}]
set_property PACKAGE_PIN D8  [get_ports {ddrDq[2]}]
set_property PACKAGE_PIN C8  [get_ports {ddrDq[3]}]
set_property PACKAGE_PIN D9  [get_ports {ddrDq[4]}]
set_property PACKAGE_PIN C9  [get_ports {ddrDq[5]}]
set_property PACKAGE_PIN E10 [get_ports {ddrDq[6]}]
set_property PACKAGE_PIN D10 [get_ports {ddrDq[7]}]
set_property PACKAGE_PIN J9  [get_ports {ddrDq[8]}]
set_property PACKAGE_PIN H9  [get_ports {ddrDq[9]}]
set_property PACKAGE_PIN J8  [get_ports {ddrDq[10]}]
set_property PACKAGE_PIN H8  [get_ports {ddrDq[11]}]
set_property PACKAGE_PIN G9  [get_ports {ddrDq[12]}]
set_property PACKAGE_PIN F9  [get_ports {ddrDq[13]}]
set_property PACKAGE_PIN G10 [get_ports {ddrDq[14]}]
set_property PACKAGE_PIN F10 [get_ports {ddrDq[15]}]
set_property PACKAGE_PIN H12 [get_ports {ddrDq[16]}]
set_property PACKAGE_PIN G12 [get_ports {ddrDq[17]}]
set_property PACKAGE_PIN K11 [get_ports {ddrDq[18]}]
set_property PACKAGE_PIN J11 [get_ports {ddrDq[19]}]
set_property PACKAGE_PIN L12 [get_ports {ddrDq[20]}]
set_property PACKAGE_PIN K12 [get_ports {ddrDq[21]}]
set_property PACKAGE_PIN J13 [get_ports {ddrDq[22]}]
set_property PACKAGE_PIN H13 [get_ports {ddrDq[23]}]
set_property PACKAGE_PIN C12 [get_ports {ddrDq[24]}]
set_property PACKAGE_PIN B12 [get_ports {ddrDq[25]}]
set_property PACKAGE_PIN C11 [get_ports {ddrDq[26]}]
set_property PACKAGE_PIN B11 [get_ports {ddrDq[27]}]
set_property PACKAGE_PIN A13 [get_ports {ddrDq[28]}]
set_property PACKAGE_PIN A12 [get_ports {ddrDq[29]}]
set_property PACKAGE_PIN D13 [get_ports {ddrDq[30]}]
set_property PACKAGE_PIN C13 [get_ports {ddrDq[31]}]
set_property PACKAGE_PIN C27 [get_ports {ddrDq[32]}]
set_property PACKAGE_PIN B27 [get_ports {ddrDq[33]}]
set_property PACKAGE_PIN E28 [get_ports {ddrDq[34]}]
set_property PACKAGE_PIN D29 [get_ports {ddrDq[35]}]
set_property PACKAGE_PIN D28 [get_ports {ddrDq[36]}]
set_property PACKAGE_PIN C28 [get_ports {ddrDq[37]}]
set_property PACKAGE_PIN A27 [get_ports {ddrDq[38]}]
set_property PACKAGE_PIN A28 [get_ports {ddrDq[39]}]
set_property PACKAGE_PIN B25 [get_ports {ddrDq[40]}]
set_property PACKAGE_PIN A25 [get_ports {ddrDq[41]}]
set_property PACKAGE_PIN C26 [get_ports {ddrDq[42]}]
set_property PACKAGE_PIN B26 [get_ports {ddrDq[43]}]
set_property PACKAGE_PIN E25 [get_ports {ddrDq[44]}]
set_property PACKAGE_PIN D25 [get_ports {ddrDq[45]}]
set_property PACKAGE_PIN D24 [get_ports {ddrDq[46]}]
set_property PACKAGE_PIN C24 [get_ports {ddrDq[47]}]
set_property PACKAGE_PIN E22 [get_ports {ddrDq[48]}]
set_property PACKAGE_PIN E23 [get_ports {ddrDq[49]}]
set_property PACKAGE_PIN B21 [get_ports {ddrDq[50]}]
set_property PACKAGE_PIN B22 [get_ports {ddrDq[51]}]
set_property PACKAGE_PIN B20 [get_ports {ddrDq[52]}]
set_property PACKAGE_PIN A20 [get_ports {ddrDq[53]}]
set_property PACKAGE_PIN D20 [get_ports {ddrDq[54]}]
set_property PACKAGE_PIN D21 [get_ports {ddrDq[55]}]
set_property PACKAGE_PIN E20 [get_ports {ddrDq[56]}]
set_property PACKAGE_PIN E21 [get_ports {ddrDq[57]}]
set_property PACKAGE_PIN F23 [get_ports {ddrDq[58]}]
set_property PACKAGE_PIN F24 [get_ports {ddrDq[59]}]
set_property PACKAGE_PIN G22 [get_ports {ddrDq[60]}]
set_property PACKAGE_PIN F22 [get_ports {ddrDq[61]}]
set_property PACKAGE_PIN H21 [get_ports {ddrDq[62]}]
set_property PACKAGE_PIN G21 [get_ports {ddrDq[63]}]
set_property IOSTANDARD SSTL15_DCI        [get_ports {ddrDq[*]}]
set_property OUTPUT_IMPEDANCE RDRV_40_40  [get_ports {ddrDq[*]}]
set_property SLEW          FAST           [get_ports {ddrDq[*]}]
set_property IBUF_LOW_PWR  FALSE          [get_ports {ddrDq[*]}]
set_property ODT           RTT_40         [get_ports {ddrDq[*]}]

set_property PACKAGE_PIN B14 [get_ports {ddrA[0]}]
set_property PACKAGE_PIN A14 [get_ports {ddrA[1]}]
set_property PACKAGE_PIN A19 [get_ports {ddrA[2]}]
set_property PACKAGE_PIN A18 [get_ports {ddrA[3]}]
set_property PACKAGE_PIN B15 [get_ports {ddrA[4]}]
set_property PACKAGE_PIN A15 [get_ports {ddrA[5]}]
set_property PACKAGE_PIN B17 [get_ports {ddrA[6]}]
set_property PACKAGE_PIN B16 [get_ports {ddrA[7]}]
set_property PACKAGE_PIN C18 [get_ports {ddrA[8]}]
set_property PACKAGE_PIN C17 [get_ports {ddrA[9]}]
set_property PACKAGE_PIN D14 [get_ports {ddrA[10]}]
set_property PACKAGE_PIN C14 [get_ports {ddrA[11]}]
set_property PACKAGE_PIN E15 [get_ports {ddrA[12]}]
set_property PACKAGE_PIN D15 [get_ports {ddrA[13]}]
set_property PACKAGE_PIN F15 [get_ports {ddrA[14]}]
set_property PACKAGE_PIN F14 [get_ports {ddrA[15]}]
set_property IOSTANDARD SSTL15_DCI        [get_ports {ddrA[*]}]
set_property OUTPUT_IMPEDANCE RDRV_40_40  [get_ports {ddrA[*]}]
set_property SLEW FAST                    [get_ports {ddrA[*]}]

set_property PACKAGE_PIN D19 [get_ports {ddrBa[0]}]
set_property PACKAGE_PIN D18 [get_ports {ddrBa[1]}]
set_property PACKAGE_PIN E16 [get_ports {ddrBa[2]}]
set_property IOSTANDARD SSTL15_DCI        [get_ports {ddrBa[*]}]
set_property OUTPUT_IMPEDANCE RDRV_40_40  [get_ports {ddrBa[*]}]
set_property SLEW FAST                    [get_ports {ddrBa[*]}]

set_property PACKAGE_PIN D16 [get_ports {ddrCsL[0]}]
set_property PACKAGE_PIN L15 [get_ports {ddrCsL[1]}]
set_property IOSTANDARD SSTL15_DCI        [get_ports {ddrCsL[*]}]
set_property OUTPUT_IMPEDANCE RDRV_40_40  [get_ports {ddrCsL[*]}]
set_property SLEW FAST                    [get_ports {ddrCsL[*]}]

set_property PACKAGE_PIN E17 [get_ports {ddrOdt[0]}]
set_property PACKAGE_PIN K15 [get_ports {ddrOdt[1]}]
set_property IOSTANDARD SSTL15_DCI        [get_ports {ddrOdt[*]}]
set_property OUTPUT_IMPEDANCE RDRV_40_40  [get_ports {ddrOdt[*]}]
set_property SLEW FAST                    [get_ports {ddrOdt[*]}]

set_property PACKAGE_PIN E18 [get_ports {ddrCke[0]}]
set_property PACKAGE_PIN H18 [get_ports {ddrCke[1]}]
set_property IOSTANDARD SSTL15_DCI        [get_ports {ddrCke[*]}]
set_property OUTPUT_IMPEDANCE RDRV_40_40  [get_ports {ddrCke[*]}]
set_property SLEW FAST                    [get_ports {ddrCke[*]}]

set_property PACKAGE_PIN C19 [get_ports {ddrCkP[0]}]
set_property PACKAGE_PIN B19 [get_ports {ddrCkN[0]}]
set_property PACKAGE_PIN H17 [get_ports {ddrCkP[1]}]
set_property PACKAGE_PIN H16 [get_ports {ddrCkN[1]}]
set_property IOSTANDARD DIFF_SSTL15_DCI   [get_ports {ddrCkP[*] ddrCkN[*]}]
set_property OUTPUT_IMPEDANCE RDRV_40_40  [get_ports {ddrCkP[*] ddrCkN[*]}]
set_property SLEW FAST                    [get_ports {ddrCkP[*] ddrCkN[*]}]

set_property -dict { PACKAGE_PIN G15 IOSTANDARD SSTL15_DCI OUTPUT_IMPEDANCE RDRV_40_40 SLEW FAST } [get_ports {ddrWeL}]
set_property -dict { PACKAGE_PIN F18 IOSTANDARD SSTL15_DCI OUTPUT_IMPEDANCE RDRV_40_40 SLEW FAST } [get_ports {ddrRasL}]
set_property -dict { PACKAGE_PIN F17 IOSTANDARD SSTL15_DCI OUTPUT_IMPEDANCE RDRV_40_40 SLEW FAST } [get_ports {ddrCasL}]
set_property -dict { PACKAGE_PIN C16 IOSTANDARD SSTL15     OUTPUT_IMPEDANCE RDRV_48_48 SLEW SLOW } [get_ports {ddrRstL}]
set_property -dict { PACKAGE_PIN K17 IOSTANDARD LVCMOS15 } [get_ports {ddrPwrEnL}]
set_property -dict { PACKAGE_PIN K18 IOSTANDARD LVCMOS15 } [get_ports {ddrAlertL}]
set_property -dict { PACKAGE_PIN J15 IOSTANDARD LVCMOS15 } [get_ports {ddrPg}]




