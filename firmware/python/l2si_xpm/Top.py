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

class Top(pr.Device):
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
        self.add(axi.AxiVersion( 
            memBase = memBase,
            offset  = 0x00000000, 
            expand  = False,
        ))

        self.add(xil.AxiSysMonUltraScale(   
            memBase = memBase,
            offset       =  0x01000000, 
            expand       =  False
        ))
        
        self.add(micron.AxiMicronN25Q(
            memBase = memBase,
            name         = "MicronN25Q",
            offset       = 0x2000000,
            addrMode     = True,                                    
            expand       = False,                                    
            hidden       = True,                                    
        ))        

        self.add(microchip.AxiSy56040(    
            memBase = memBase,
            offset       =  0x03000000, 
            expand       =  False,
            description  = "\n\
                Timing Crossbar:  https://confluence.slac.stanford.edu/x/m4H7D   \n\
                -----------------------------------------------------------------\n\
                OutputConfig[0] = 0x0: Connects RTM_TIMING_OUT0 to RTM_TIMING_IN0\n\
                OutputConfig[0] = 0x1: Connects RTM_TIMING_OUT0 to FPGA_TIMING_IN\n\
                OutputConfig[0] = 0x2: Connects RTM_TIMING_OUT0 to BP_TIMING_IN\n\
                OutputConfig[0] = 0x3: Connects RTM_TIMING_OUT0 to RTM_TIMING_IN1\n\
                -----------------------------------------------------------------\n\
                OutputConfig[1] = 0x0: Connects FPGA_TIMING_OUT to RTM_TIMING_IN0\n\
                OutputConfig[1] = 0x1: Connects FPGA_TIMING_OUT to FPGA_TIMING_IN\n\
                OutputConfig[1] = 0x2: Connects FPGA_TIMING_OUT to BP_TIMING_IN\n\
                OutputConfig[1] = 0x3: Connects FPGA_TIMING_OUT to RTM_TIMING_IN1 \n\
                -----------------------------------------------------------------\n\
                OutputConfig[2] = 0x0: Connects Backplane DIST0 to RTM_TIMING_IN0\n\
                OutputConfig[2] = 0x1: Connects Backplane DIST0 to FPGA_TIMING_IN\n\
                OutputConfig[2] = 0x2: Connects Backplane DIST0 to BP_TIMING_IN\n\
                OutputConfig[2] = 0x3: Connects Backplane DIST0 to RTM_TIMING_IN1\n\
                -----------------------------------------------------------------\n\
                OutputConfig[3] = 0x0: Connects Backplane DIST1 to RTM_TIMING_IN0\n\
                OutputConfig[3] = 0x1: Connects Backplane DIST1 to FPGA_TIMING_IN\n\
                OutputConfig[3] = 0x2: Connects Backplane DIST1 to BP_TIMING_IN\n\
                OutputConfig[3] = 0x3: Connects Backplane DIST1 to RTM_TIMING_IN1\n\
                -----------------------------------------------------------------\n"\
            ))
                            
        self.add(ti.AxiCdcm6208(     
            memBase = memBase,
            offset       =  0x05000000, 
            expand       =  False,
        ))

        self.add(xpm.TimingFrameRx(
            memBase = memBase,
            name = 'UsTiming',
            offset = 0x08000000,
        ))

        self.add(xpm.TimingFrameRx(
            memBase = memBase,
            name = 'CuTiming',
            offset = 0x08400000,
        ))

        self.add(xpm.CuGenerator(
            memBase = memBase,
            name = 'CuGenerator',
            offset = 0x08800000,
        ))

        
        for i in range(len(Top.mmcmParms)):
            self.add(xpm.MmcmPhaseLock(
                memBase = memBase,
                name   = Top.mmcmParms[i][0],
                offset = Top.mmcmParms[i][1],
            ))
        
        hsrParms = [ ['HSRep0',0x09000000],
                     ['HSRep1',0x09010000],
                     ['HSRep2',0x09020000],
                     ['HSRep3',0x09030000],
                     ['HSRep4',0x09040000],
                     ['HSRep5',0x09050000] ]
        for i in range(len(hsrParms)):
            self.add(xpm.Ds125br401(
                memBase = memBase,
                name   = hsrParms[i][0],
                offset = hsrParms[i][1],
            ))

        self.amcs = []
        for i in range(2):
            amc = xpm.MpsSfpAmc(
                memBase = memBase,
                name    = 'Amc%d'%i,
                offset  = 0x09000000+(i+1)*0x100000,
            )
            self.add(amc)
            self.amcs.append(amc)

#        self.add(timing.GthRxAlignCheck(
        self.add(xpm.GthRxAlignCheck(
            memBase = memBase,
            name   = 'UsGthRxAlign',
            offset = 0x0b000000,
        ))        
                       
#        self.add(timing.GthRxAlignCheck(
        self.add(xpm.GthRxAlignCheck(
            memBase = memBase,
            name   = 'CuGthRxAlign',
            offset = 0x0c000000,
        ))        
                       
        self.add(xpm.XpmApp(
            memBase = memBase,
            name   = 'XpmApp',
            offset = 0x80000000,
        ))
        
        self.add(AxiLiteRingBuffer(
            memBase = memBase,
            name      = 'AxiLiteRingBuffer',
            datawidth = 16,
            offset    = 0x80010000,
        ))

        self.add(xpm.XpmSequenceEngine(
            memBase = memBase,
            name   = 'SeqEng_0',
            offset = 0x80040000,
        ))

#        self.add(xpm.CuPhase(
#            memBase = memBase,
#            name = 'CuPhase',
#            offset = 0x80050000,
#        ))

        if False:
            self.add(xpm.XpmPhase(
                memBase = memBase,
                name   = 'CuToScPhase',
                offset = 0x80080000,
            ))

