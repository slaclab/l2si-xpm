create_generated_clock -name timingPhyClk [get_pins U_Base/U_Core/TimingGtCoreWrapper_1/LOCREF_G.TIMING_TXCLK_BUFG_GT/O]
create_generated_clock -name iusRefClk [get_pins U_Base/U_Core/TIMING_REFCLK_IBUFDS_GTE3/ODIV2]

set_clock_groups -asynchronous \
                 -group [get_clocks timingPhyClk] \
                 -group [get_clocks iusRefClk]
