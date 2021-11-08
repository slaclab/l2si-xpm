#!/usr/bin/env python3
#-----------------------------------------------------------------------------
# This file is part of the 'Camera link gateway'. It is subject to
# the license terms in the LICENSE.txt file found in the top-level directory
# of this distribution and at:
#    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
# No part of the 'Camera link gateway', including this file, may be
# copied, modified, propagated, or distributed except according to the terms
# contained in the LICENSE.txt file.
#-----------------------------------------------------------------------------
import pyrogue as pr
import rogue
import click

import axipcie                as pcie
import l2si_xpm               as xpm

rogue.Version.minVersion('5.1.0')
# rogue.Version.exactVersion('5.1.0')

class DevRoot(pr.Root):

    def __init__(self,
                 dev            = None,
                 pollEn         = True,  # Enable automatic polling registers
                 initRead       = True,  # Read all registers at start of the system
                 enableConfig   = True,
                 enVcMask       = 0x3, # Enable lane mask
                 **kwargs):

        # Set local variables
        self.dev            = dev
        self.enableConfig   = enableConfig
        self.defaultFile    = []

        # Check for simulation
        if dev == 'sim':
            kwargs['timeout'] = 100000000 # 100 s
        else:
            kwargs['timeout'] = 5000000 # 5 s

        # Pass custom value to parent via super function
        super().__init__(
            pollEn      = pollEn,
            initRead    = initRead,
            **kwargs)

        # Unhide the RemoteVariableDump command
        self.RemoteVariableDump.hidden = False

        # Create memory interface
        self.memMap = axipcie.createAxiPcieMemMap(dev, 'localhost', 8000)
        self.memMap.setName('PCIe_Bar0')

        # Instantiate the top level Device and pass it the memory map
        self.add(pcie.AxiPcieCore(
            memBase     = self.memMap,
            boardType   = 'Kcu1500',
            useSpi      = True,
            numDmaLanes = 1,
            offset      = 0x0000_0000,
        ))

        self.add(xpm.Top(
            name       = 'XPM',
            memBase    = self.memMap,
            offset     = 0x0080_0000,
        ))

        # Create empty list
        self.dmaStreams     = [[None for x in range(4)] for y in range(4)]
        self._srp           = [None for x in range(4)]
        self._dbg           = [None for x in range(4)]
        self.enVcMask       = [False for x in range(4)]

        # Create DMA streams
        lane = 0
        if True:
            for vc in range(4):
                if enVcMask & (0x1 << vc):
                    self.enVcMask[vc] = True
                    if (dev is not 'sim'):
                        self.dmaStreams[lane][vc] = rogue.hardware.axi.AxiStreamDma(dev,(0x100*lane)+vc,1)
                    else:
                        self.dmaStreams[lane][vc] = rogue.interfaces.stream.TcpClient('localhost', (8000+2)+(512*lane)+2*vc)

        # Check if not doing simulation
        if (dev is not 'sim'):

            # Create the stream interface
            lane = 0
            if True:

                # Check if PGP[lane].VC[0] = SRPv3 (register access) is enabled
                if self.enVcMask[0]:
                    # Create the SRPv3
                    self._srp[lane] = rogue.protocols.srp.SrpV3()
                    self._srp[lane].setName(f'SRPv3[{lane}]')
                    # Connect DMA to SRPv3
                    self.dmaStreams[lane][0] = self._srp[lane]

        # Create the stream interface
        lane = 0
        if True:

            # Check if PGP[lane].VC[1] = Camera Image (streaming data) is enabled
            if self.enVcMask[1]:

                # Debug slave
                if dataDebug:
                    # Connect the streams
                    self.dmaStreams[lane][1] >> self._dbg[lane]

        # Check if PGP[lane].VC[0] = SRPv3 (register access) is enabled
        if self.enVcMask[0]:
            self.add(pr.LocalVariable(
                name        = 'RunState',
                description = 'Run state status, which is controlled by the StopRun() and StartRun() commands',
                mode        = 'RO',
                value       = False,
            ))

    def start(self, **kwargs):
        super().start(**kwargs)

#        # Hide all the "enable" variables
#        for enableList in self.find(typ=pr.EnableVariable):
#            # Hide by default
#            enableList.hidden = True

        # Check if simulation
        if (self.dev is 'sim'):
            pass

        # Check if PGP[lane].VC[0] = SRPv3 (register access) is enabled
        elif self.enVcMask[0]:
            self.ReadAll()
            self.ReadAll()

            # Load the configurations
            if self.enableConfig:

                # Read all the variables
                self.ReadAll()
                self.ReadAll()

                # Load the YAML configurations
                defaultFile.extend(self.defaultFile)
                print(f'Loading {defaultFile} Configuration File...')
                self.LoadConfig(defaultFile)

    # Function calls after loading YAML configuration
    def initialize(self):
        super().initialize()
        self.CountReset()
