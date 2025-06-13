-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- This file is part of 'LCLS Timing Core'.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'LCLS Timing Core', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;


library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;

library lcls_timing_core;
use lcls_timing_core.TPGPkg.all;

entity TPGMiniReg is
   generic (
      TPD_G            : time            := 1 ns;
      USE_WSTRB_G      : boolean         := false);
   port (
      -- PCIe Interface
      irqActive      : in  sl;
      irqEnable      : out sl;
      irqReq         : out sl;
      -- AXI-Lite Interface
      axiReadMaster  : in  AxiLiteReadMasterType;
      axiReadSlave   : out AxiLiteReadSlaveType;
      axiWriteMaster : in  AxiLiteWriteMasterType;
      axiWriteSlave  : out AxiLiteWriteSlaveType;
      -- EVR Interface
      status         : in  TPGStatusType;
      config         : out TPGConfigType;
      txReset        : out sl;
      txLoopback     : out slv(2 downto 0);
      txInhibit      : out sl;
      -- Clock and Reset
      axiClk         : in  sl;
      axiRst         : in  sl);
end TPGMiniReg;

architecture rtl of TPGMiniReg is

   type RegType is record
     pulseId           : slv(31 downto 0);
     timeStamp         : slv(31 downto 0);
     countUpdate       : sl;
     FixedRateDivisors : Slv20Array(9 downto 0);
     ACRateDivisors    : Slv8Array(5 downto 0);
     config            : TPGConfigType;
     txReset           : sl;
     txLoopback        : slv( 2 downto 0);
     txInhibit         : sl;
     rdData            : slv(31 downto 0);
     axiReadSlave      : AxiLiteReadSlaveType;
     axiWriteSlave     : AxiLiteWriteSlaveType;
   end record RegType;

   constant REG_INIT_C : RegType := (
     pulseId           => (others=>'0'),
     timeStamp         => (others=>'0'),
     countUpdate       => '0',
     FixedRateDivisors => TPG_CONFIG_INIT_C.FixedRateDivisors,
     ACRateDivisors    => TPG_CONFIG_INIT_C.ACRateDivisors,
     config            => TPG_CONFIG_INIT_C,
     txReset           => '0',
     txLoopback        => "000",
     txInhibit         => '0',
     rdData            => (others=>'0'),
     axiReadSlave      => AXI_LITE_READ_SLAVE_INIT_C,
     axiWriteSlave     => AXI_LITE_WRITE_SLAVE_INIT_C);

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

begin

   -------------------------------
   -- Configuration Register
   -------------------------------
   comb : process (axiReadMaster, axiRst, axiWriteMaster, irqActive, r, status) is
     variable v            : RegType;
     variable tmp          : sl;
     variable ep           : AxiLiteEndPointType;
   begin
      -- Latch the current value
      v := r;

      -- Reset strobing signals
      v.config.pulseIdWrEn   := '0';
      v.config.timeStampWrEn := '0';
      v.config.intervalRst   := '0';
      v.txReset              := '0';

      axiSlaveWaitTxn(ep, axiWriteMaster, axiReadMaster, v.axiWriteSlave, v.axiReadSlave);

      axiSlaveRegister( ep, x"000", 0, v.config.clock_divisor );
      axiSlaveRegister( ep, x"000", 8, v.config.clock_remainder );
      axiSlaveRegister( ep, x"000",16, v.config.clock_step );
      axiSlaveRegister( ep, x"004", 0, v.config.baseDivisor );

      axiSlaveRegisterR( ep, x"008", 0, status.pulseId );
      axiSlaveRegisterR( ep, x"010", 0, status.timeStamp );
      
      for i in 0 to 9 loop
        axiSlaveRegister( ep, toSlv(24+4*i,12), 0, v.FixedRateDivisors(i) );
      end loop;
      tmp := '0';
      axiWrDetect(ep, x"040",tmp);
      if tmp='1' then
        v.config.FixedRateDivisors := r.FixedRateDivisors;
      end if;

      axiSlaveRegisterR( ep, x"04C", 0, status.nbeamseq );
      axiSlaveRegisterR( ep, x"04C", 6, status.nexptseq );
      axiSlaveRegisterR( ep, x"04C",22, status.seqaddrlen );
      axiSlaveRegisterR( ep, x"04C",26, status.nallowseq );
      axiSlaveRegister( ep, x"058", 0, v.config.pulseId );
      axiSlaveRegister( ep, x"060", 0, v.config.timeStamp );

      axiSlaveRegister( ep, x"068", 0, v.txreset );
      axiSlaveRegister( ep, x"06C", 0, v.config.intervalRst );
      axiSlaveRegister( ep, x"070", 0, v.config.pulseIdWrEn );
      axiSlaveRegister( ep, x"074", 0, v.config.timeStampWrEn );
      
      for i in 0 to 5 loop
        axiSlaveRegister( ep, toSlv(128+4*i,12), 0, v.ACRateDivisors(i) );
      end loop;
      tmp := '0';
      axiWrDetect(ep, x"098",tmp);
      if tmp='1' then
        v.config.ACRateDivisors := r.ACRateDivisors;
      end if;

      axiSlaveRegisterR( ep, x"500", 0, status.pllChanged );
      axiSlaveRegisterR( ep, x"504", 0, status.count186M );
      axiSlaveRegisterR( ep, x"508", 0, status.countSyncE );
      axiSlaveRegister ( ep, x"50C", 0, v.config.interval);
      axiSlaveRegisterR( ep, x"510", 0, status.countBRT );

      axiSlaveDefault(ep, v.axiWriteSlave, v.axiReadSlave);

      -- Synchronous Reset
      --if axiRst = '1' then
      --  v := REG_INIT_C;
      --end if;

      -- Register the variable for next clock cycle
      rin <= v;

      -- Outputs
      axiWriteSlave   <= r.axiWriteSlave;
      axiReadSlave    <= r.axiReadSlave;
      config          <= r.config;
      txReset         <= r.txReset;
      txLoopback      <= r.txLoopback;
      txInhibit       <= r.txInhibit;

      irqEnable       <= '0';
      irqReq          <= '0';
   end process comb;

   seq : process (axiClk) is
   begin
      if rising_edge(axiClk) then
         r <= rin after TPD_G;
      end if;
   end process seq;

end rtl;
