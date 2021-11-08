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
import surf.axi                     as axi
import surf.xilinx                  as xil
import surf.devices.ti              as ti
import surf.devices.micron          as micron
import surf.devices.microchip       as microchip
import LclsTimingCore               as timing
import l2si_xpm                     as xpm
from l2si_xpm._AxiLiteRingBuffer import AxiLiteRingBuffer

class Si570(pr.Device):
    def __init__(self,**kwargs):
        super().__init__(**kwargs)

        self.addRemoteVariables(
            name         = 'Regs',
            description  = 'Registers',
            offset       = (0 << 2),
            bitSize      = 8,
            mode         = 'RO',
            number       = 256,
            stride       = 4,
            hidden       = True
        )

    def reset(self):
        v = self.Regs[135].get() | 1
        self.Regs[135].set(v)
        v = 1
        while (v&1):
            time.sleep(100e-6)
            v = self.Regs[135]

    def read(self):
        #  Read factory calibration for 156.25 MHz
        hsd_divn = [4,5,6,7,0,9,0,11]
        v = self.Regs[7]
        hs_dev = hsd_devn[(v>>5)&7]
        n1 = (v&0x1f)<<2
        v = self.Regs[8]
        n1 |= (v>>6)&3
        rfreq = v&0x3f
        for i in range(9,13):
            v = self.Regs[i]
            rfreq <<= 8
            rfreq |= (v&0xff)

        f = (156.25 * float(hs_div * (n1+1))) * float(1<<28)/ float(rfreq)
        return f

    def program(self,index):
        _hsd_div = [ 7, 3 ]
        _n1      = [ 3, 3 ]
        _rfreq   = [ 5236., 5200. ]
        self.reset();

        fcal = self.read()

        #  Program for 1300/7 MHz

        #  Freeze DCO
        v = self.Regs[137].get() | (1<<4)
        self.Regs[137].set(v)

        hs_div = _hsd_div[index]
        n1     = _n1     [index]
        rfreq  = int(_rfreq[index] / fcal * double(1<<28));

        self.Regs[7].set( ((hs_div&7)<<5) | ((n1>>2)&0x1f) )
        self.Regs[8].set( ((n1&3)    <<6) | ((rfreq>>32)&0x3f) )
        self.Regs[9].set( (rfreq>>24)&0xff )
        self.Regs[10].set( (rfreq>>16)&0xff )
        self.Regs[11].set( (rfreq>>8)&0xff )
        self.Regs[12].set( (rfreq>>0)&0xff )
  
        #  Unfreeze DCO
        v = self.Regs[137].get() & ~(1<<4)
        self.Regs[137].set(v)

        v = self.Regs[135].get() | (1<<6)
        self.Regs[135].set(v)

        self.read()


class DevPcie(pr.Device):
    mmcmParms = [ ['MmcmPL119', 0x08900000],
                  ['MmcmPL70' , 0x08a00000],
                  ['MmcmPL130', 0x08b00000],
                  ['MmcmPL186', 0x80040000] ]

    def __init__(   self,       
            name        = "Top",
            description = "Container for XPM",
            memBase     = 0,
            **kwargs):
        super().__init__(name=name, description=description, **kwargs)
        
        ######################################################################
        
        # Add devices
        core = axi.AxiPcieCore(
            boardType = 'Kcu1500',
            memBase = memBase,
            offset  = 0x00000000, 
            expand  = False,
        )
        core.add(Si570(
                name    = 'Si570',
                offset  = 0x72_000,
                memBase = core.AxilBridge.proxy,
                enabled = False,
        ))
        self.add(core)

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
            offset = 0x00820000,
        ))

#        self.add(xpm.CuPhase(
#            memBase = memBase,
#            name = 'CuPhase',
#            offset = 0x00850000,
#        ))

        self.add(xpm.XpmPhase(
            memBase = memBase,
            name   = 'CuToScPhase',
            offset = 0x00850000,
        ))

