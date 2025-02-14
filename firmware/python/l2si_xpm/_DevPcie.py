#!/usr/bin/env python3
##############################################################################
## This file is part of 'EPIX'.
## It is subject to the license terms in the LICENSE.txt file found in the 
## top-level directory of this distribution and at: 
##    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
## No part of 'EPIX', including this file, 
## may be copied, modified, propagated, or distributed except according to 
## the terms contained in the LICENSE.txt file.
##############################################################################

import pyrogue as pr
import pyrogue.protocols
import time
import axipcie                      as axipcie
import surf.axi                     as axi
import surf.xilinx                  as xil
import surf.devices.ti              as ti
import surf.devices.micron          as micron
import surf.devices.microchip       as microchip
import LclsTimingCore               as timing
import l2si_xpm                     as xpm
from l2si_xpm._AxiLiteRingBuffer import AxiLiteRingBuffer

class DevReset(pr.Device):
    def __init__(   self, 
            name        = "DevReset", 
            description = "Device Reset Control", 
            **kwargs):
        super().__init__(name=name, description=description, **kwargs)

        self.add(pr.RemoteVariable(
            name         = 'clearTimingPhyReset',
            description  = "Clear timingPhyRst",
            offset       = 0x0100,
            bitSize      =  1,
            bitOffset    =  0x00,
            base         = pr.UInt,
            mode         = "RW",
        ))




class DevPcie(pr.Device):
    mmcmParms = [ ['MmcmPL119', 0x08900000],
                  ['MmcmPL70' , 0x08a00000],
                  ['MmcmPL130', 0x08b00000],
                  ['MmcmPL186', 0x80040000] ]

    def __init__(   self,       
                    name        = "Top",
                    description = "Container for XPM",
                    memBase     = 0,
                    isXpmGen    = True,
                    **kwargs):
        super().__init__(name=name, description=description, **kwargs)
        self.isXpmGen = isXpmGen
        ######################################################################
        
        # Add devices
        self.add(xpm.AxiPcieCore(
            boardType = 'Kcu1500',
            memBase = memBase,
            offset  = 0x00000000, 
            expand  = False,
        ))

        self.add(xpm.XpmApp(
            memBase = memBase,
            name   = 'XpmApp',
            offset = 0x00800000,
        ))
        
        self.add(AxiLiteRingBuffer(
            memBase = memBase,
            name      = 'AxiLiteRingBuffer',
            datawidth = 16,
            offset    = 0x00810000,
        ))

        self.add(xpm.XpmSequenceEngine(
            memBase = memBase,
            name   = 'SeqEng_0',
            offset = 0x00840000,
        ))

        self.add(timing.TPGMiniCore(
            memBase = memBase,
            name   = 'TpgMini',
            offset = 0x00830000,
        ))

#        self.add(xpm.CuPhase(
#            memBase = memBase,
#            name = 'CuPhase',
#            offset = 0x00850000,
#        ))

        self.add(DevReset(
            memBase      = memBase,
            name         = 'DevReset',
            offset       = 0x00820000,
        ))


    def start(self):
        #  Reprogram the reference clock
        self.DevReset.clearTimingPhyReset.set(0)
        time.sleep(0.01)
        self.AxiPcieCore.I2cMux.set(1<<2)
        self.AxiPcieCore.Si570._program()
        time.sleep(0.01)
        #  Reset the Tx and Rx PLLs
        for i in range(8):
            self.XpmApp.link.set(i)
            #  Need to refresh these fields that share a 32-bit register :(
            self.XpmApp.fullMask.get()
            self.XpmApp.txReset .get()
            self.XpmApp.rxReset .get()
            self.XpmApp.fullEn  .get()
            #  End refresh
            self.XpmApp.txPllReset.set(1)
            time.sleep(0.01)
            self.XpmApp.txPllReset.set(0)
            time.sleep(0.01)
            self.XpmApp.rxPllReset.set(1)
            time.sleep(0.01)
            self.XpmApp.rxPllReset.set(0)
        self.XpmApp.link.set(0)
        time.sleep(0.01)
        self.DevReset.clearTimingPhyReset.set(1)

        if self.isXpmGen:
            #  Set the fixed rate markers
            self.TpgMini.FixedRateDiv[0].set(910000)
            self.TpgMini.FixedRateDiv[1].set( 91000)
            self.TpgMini.FixedRateDiv[2].set(  9100)
            self.TpgMini.FixedRateDiv[3].set(   910)
            self.TpgMini.FixedRateDiv[4].set(    91)
            self.TpgMini.FixedRateDiv[5].set(    13)
            self.TpgMini.FixedRateDiv[6].set(     1)
            self.TpgMini.RateReload.set(1)

        #  Reset to realign the rate markers
        self.DevReset.clearTimingPhyReset.set(0)
        time.sleep(0.001)
        self.DevReset.clearTimingPhyReset.set(1)

            
        
