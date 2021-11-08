#!/usr/bin/env python

import rogue
import rogue.hardware.axi

import sys
import pyrogue as pr
import argparse

import logging

from p4p.server import Server, StaticProvider
from threading import Lock

import time
from datetime import datetime
#import pdb

#import psdaq.pyxpm.xpm as xpmr
from psdaq.pyxpm.xpm.Top import *
from psdaq.pyxpm.pvstats import *
from psdaq.pyxpm.pvctrls import *
from psdaq.pyxpm.pvxtpg  import *
from psdaq.pyxpm.pvhandler import *

class NoLock(object):
    def __init__(self):
        self._level=0
        self._lock = Lock()

    def acquire(self):
        if self._level!=0:
            print('NoLock.acquire level {}'.format(self._level))
        self._level=self._level+1

    def release(self):
        if self._level!=1:
            print('NoLock.release level {}'.format(self._level))
        self._level=self._level-1

def main():
    global pvdb
    pvdb = {}     # start with empty dictionary
    global prefix
    prefix = ''

    parser = argparse.ArgumentParser(prog=sys.argv[0], description='host PVs for XPM')

    parser.add_argument('-P', required=True, help='e.g. DAQ:LAB2:XPM:1', metavar='PREFIX')
    parser.add_argument('-v', '--verbose', action='store_true', help='be verbose')
    parser.add_argument('--ip', type=str, required=True, help="IP address" )
    parser.add_argument('--db', type=str, default=None, help="save/restore db, for example [https://pswww.slac.stanford.edu/ws-auth/devconfigdb/ws/,configDB,LAB2,PROD]")
    parser.add_argument('-I', action='store_true', help='initialize Cu timing')
    parser.add_argument('-L', action='store_true', help='bypass AMC Locks')
    parser.add_argument('-F', type=float, default=1.076923e-6, help='fiducial period (sec)')

    args = parser.parse_args()
    if args.verbose:
#        logging.basicConfig(level=logging.DEBUG)
        setVerbose(True)

    # Set base
    base = pr.Root(name='AMCc',description='') 


    ################################################################################################################
    # UDP_SRV_XVC_IDX_C         => 2542,  -- Xilinx XVC 
    # UDP_SRV_SRPV0_IDX_C       => 8192,  -- Legacy SRPv0 register access (still used for remote FPGA reprogramming)
    # UDP_SRV_RSSI0_IDX_C       => 8193,  -- Legacy Non-interleaved RSSI for Register access and ASYNC messages
    # UDP_SRV_RSSI1_IDX_C       => 8194,  -- Legacy Non-interleaved RSSI for bulk data transfer
    # UDP_SRV_BP_MGS_IDX_C      => 8195,  -- Backplane Messaging
    # UDP_SRV_TIMING_IDX_C      => 8197,  -- Timing ASYNC Messaging
    # UDP_SRV_RSSI_ILEAVE_IDX_C => 8198);  -- Interleaved RSSI         
    ################################################################################################################

    rudp = pyrogue.protocols.UdpRssiPack( name='rudpReg', host=ipAddr, port=8193, packVer = 1, jumbo = False)

    # Connect the SRPv3 to tDest = 0x0
    srp = rogue.protocols.srp.SrpV3()
    pr.streamConnectBiDir( srp, rudp.application(dest=0x0) )

    # Create stream interface
    stream = pr.protocols.UdpRssiPack( name='rudpData', host=ipAddr, port=8194, packVer = 1, jumbo = False)

    base.add(Top(
        name    = 'XPM',
        memBase = srp
    ))
    
    # Start the system
    base.start(
#        pollEn   = False,
#        initRead = False,
#        zmqPort  = None,
    )

    xpm = base.XPM
    app = base.XPM.XpmApp

    # Print the AxiVersion Summary
    xpm.AxiVersion.printStatus()

    provider = StaticProvider(__name__)

    lock = Lock()

    pvstats = PVStats(provider, lock, args.P, xpm, args.F)
#    pvctrls = PVCtrls(provider, lock, name=args.P, ip=args.ip, xpm=xpm, stats=pvstats._groups, handle=pvstats.handle, db=args.db, cuInit=True)
    pvctrls = PVCtrls(provider, lock, name=args.P, ip=args.ip, xpm=xpm, stats=pvstats._groups, handle=pvstats.handle, db=args.db, cuInit=args.I)
    pvxtpg  = PVXTpg(provider, lock, args.P, xpm, xpm.mmcmParms, cuMode='xtpg' in xpm.AxiVersion.ImageName.get(), bypassLock=args.L)

    # process PVA transactions
    updatePeriod = 1.0
    with Server(providers=[provider]):
        try:
            if pvxtpg is not None:
                pvxtpg .init()
            pvstats.init()
            while True:
                prev = time.perf_counter()
                if pvxtpg is not None:
                    pvxtpg .update()
                pvstats.update()
                pvctrls.update()
                curr  = time.perf_counter()
                delta = prev+updatePeriod-curr
#                print('Delta {:.2f}  Update {:.2f}  curr {:.2f}  prev {:.2f}'.format(delta,curr-prev,curr,prev))
                if delta>0:
                    time.sleep(delta)
        except KeyboardInterrupt:
            pass

if __name__ == '__main__':
    main()
