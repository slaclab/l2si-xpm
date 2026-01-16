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
      axiClk         : in  sl;
      axiRst         : in  sl;
      axiReadMaster  : in  AxiLiteReadMasterType;
      axiReadSlave   : out AxiLiteReadSlaveType;
      axiWriteMaster : in  AxiLiteWriteMasterType;
      axiWriteSlave  : out AxiLiteWriteSlaveType;
      -- EVR Interface
      clk            : in  sl;
      rst            : in  sl;
      status         : in  TPGStatusType;
      config         : out TPGConfigType;
      txReset        : out sl;
      txLoopback     : out slv(2 downto 0);
      txInhibit      : out sl );
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

   signal statusS    : TPGStatusType;

begin

   -------------------------------
   -- Configuration Register
   -------------------------------
   comb : process (axiReadMaster, axiRst, axiWriteMaster, r, statusS) is
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

      axiSlaveRegisterR( ep, x"008", 0, statusS.pulseId );
      axiSlaveRegisterR( ep, x"010", 0, statusS.timeStamp );
      
      for i in 0 to 9 loop
        axiSlaveRegister( ep, toSlv(24+4*i,12), 0, v.FixedRateDivisors(i) );
      end loop;
      tmp := '0';
      axiWrDetect(ep, x"040",tmp);
      if tmp='1' then
        v.config.FixedRateDivisors := r.FixedRateDivisors;
      end if;

      axiSlaveRegisterR( ep, x"04C", 0, statusS.nbeamseq );
      axiSlaveRegisterR( ep, x"04C", 6, statusS.nexptseq );
      axiSlaveRegisterR( ep, x"04C",22, statusS.seqaddrlen );
      axiSlaveRegisterR( ep, x"04C",26, statusS.nallowseq );
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

      axiSlaveRegisterR( ep, x"500", 0, statusS.pllChanged );
      axiSlaveRegisterR( ep, x"504", 0, statusS.count186M );
      axiSlaveRegisterR( ep, x"508", 0, statusS.countSyncE );
      axiSlaveRegister ( ep, x"50C", 0, v.config.interval);
      axiSlaveRegisterR( ep, x"510", 0, statusS.countBRT );

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

      irqEnable       <= '0';
      irqReq          <= '0';
   end process comb;

   seq : process (axiClk) is
   begin
      if rising_edge(axiClk) then
         r <= rin after TPD_G;
      end if;
   end process seq;

   --  config records
   U_Config_clock_divisor : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => config.clock_divisor'length )
     port map (
       wr_clk => axiClk,
       din    => r.config.clock_divisor,
       rd_clk => clk,
       dout   => config.clock_divisor);

   U_Config_clock_remainder : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => config.clock_remainder'length )
     port map (
       wr_clk => axiClk,
       din    => r.config.clock_remainder,
       rd_clk => clk,
       dout   => config.clock_remainder);

   U_Config_clock_step : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => config.clock_step'length )
     port map (
       wr_clk => axiClk,
       din    => r.config.clock_step,
       rd_clk => clk,
       dout   => config.clock_step);

   U_Config_baseDivisor : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => config.baseDivisor'length )
     port map (
       wr_clk => axiClk,
       din    => r.config.baseDivisor,
       rd_clk => clk,
       dout   => config.baseDivisor);

   U_Config_pulseId : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => config.pulseId'length )
     port map (
       wr_clk => axiClk,
       din    => r.config.pulseId,
       rd_clk => clk,
       dout   => config.pulseId);

   U_Config_timeStamp : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => config.timeStamp'length )
     port map (
       wr_clk => axiClk,
       din    => r.config.timeStamp,
       rd_clk => clk,
       dout   => config.timeStamp);

   U_Config_pulseIdWrEn : entity surf.Synchronizer
     port map (
       clk     => clk,
       dataIn  => r.config.pulseIdWrEn,
       dataOut => config.pulseIdWrEn);

   U_Config_timeStampWrEn : entity surf.Synchronizer
     port map (
       clk     => clk,
       dataIn  => r.config.timeStampWrEn,
       dataOut => config.timeStampWrEn);

   U_Config_interval : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => config.interval'length )
     port map (
       wr_clk => axiClk,
       din    => r.config.interval,
       rd_clk => clk,
       dout   => config.interval);

   GEN_FRD : for i in 0 to 9 generate
     U_Config_FixedRateDivisor : entity surf.SynchronizerFifo
       generic map (
         DATA_WIDTH_G => 20 )
       port map (
         wr_clk   => axiClk,
         din      => r.config.FixedRateDivisors(i),
         rd_clk   => clk,
         dout     => config.FixedRateDivisors(i) );
   end generate GEN_FRD;
   
   GEN_ACD : for i in 0 to 5 generate
     U_Config_ACRateDivisor : entity surf.SynchronizerFifo
       generic map (
         DATA_WIDTH_G => 8 )
       port map (
         wr_clk   => axiClk,
         din      => r.config.ACRateDivisors(i),
         rd_clk   => clk,
         dout     => config.ACRateDivisors(i) );
   end generate GEN_ACD;

   U_Config_txReset : entity surf.Synchronizer
     port map (
       clk      => clk,
       dataIn   => r.txReset,
       dataOut  => txReset );
   
   U_Config_txLoopback : entity surf.SynchronizerVector
     generic map (
       WIDTH_G => 3 )
     port map (
       clk      => clk,
       dataIn   => r.txLoopback,
       dataOut  => txLoopback );

   U_Config_txInhibit : entity surf.Synchronizer
     port map (
       clk      => clk,
       dataIn   => r.txInhibit,
       dataOut  => txInhibit );
   
   -- status
   U_Status_pulseId : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => status.pulseId'length )
     port map (
       wr_clk => clk,
       din    => status.pulseId,
       rd_clk => axiClk,
       dout   => statusS.pulseId);

   U_Status_timeStamp : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => status.timeStamp'length )
     port map (
       wr_clk => clk,
       din    => status.timeStamp,
       rd_clk => axiClk,
       dout   => statusS.timeStamp);

   U_Status_count186M : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => status.count186M'length )
     port map (
       wr_clk => clk,
       din    => status.count186M,
       rd_clk => axiClk,
       dout   => statusS.count186M);

   U_Status_countBRT : entity surf.SynchronizerFifo
     generic map (
       DATA_WIDTH_G => status.countBRT'length )
     port map (
       wr_clk => clk,
       din    => status.countBRT,
       rd_clk => axiClk,
       dout   => statusS.countBRT);

   U_Status_nbeamseq : entity surf.SynchronizerVector
     generic map (
       WIDTH_G => status.nbeamseq'length )
     port map (
       clk     => axiClk,
       dataIn  => status.nbeamseq,
       dataOut => statusS.nbeamseq);

   U_Status_nexptseq : entity surf.SynchronizerVector
     generic map (
       WIDTH_G => status.nexptseq'length )
     port map (
       clk     => axiClk,
       dataIn  => status.nexptseq,
       dataOut => statusS.nexptseq);

   U_Status_seqaddrlen : entity surf.SynchronizerVector
     generic map (
       WIDTH_G => status.seqaddrlen'length )
     port map (
       clk     => axiClk,
       dataIn  => status.seqaddrlen,
       dataOut => statusS.seqaddrlen);

   U_Status_nallowseq : entity surf.SynchronizerVector
     generic map (
       WIDTH_G => status.nallowseq'length )
     port map (
       clk     => axiClk,
       dataIn  => status.nallowseq,
       dataOut => statusS.nallowseq);

   U_Status_pllChanged : entity surf.SynchronizerVector
     generic map (
       WIDTH_G => status.pllChanged'length )
     port map (
       clk     => axiClk,
       dataIn  => status.pllChanged,
       dataOut => statusS.pllChanged);

   U_Status_countSyncE : entity surf.SynchronizerVector
     generic map (
       WIDTH_G => status.countSyncE'length )
     port map (
       clk     => axiClk,
       dataIn  => status.countSyncE,
       dataOut => statusS.countSyncE);
   
end rtl;
