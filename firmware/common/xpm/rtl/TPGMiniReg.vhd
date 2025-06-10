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
use lcls_timing_core.TPGMiniEdefPkg.all;

entity TPGMiniReg is
   generic (
      TPD_G            : time            := 1 ns;
      NARRAYS_BSA      : integer         := 1;
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
      edefConfig     : out TPGMiniEdefConfigType;
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
     bsadefRateMode    : Slv2Array (NARRAYS_BSA-1 downto 0);
     bsadefFixedRate   : Slv4Array (NARRAYS_BSA-1 downto 0);
     bsadefACRate      : Slv3Array (NARRAYS_BSA-1 downto 0);
     bsadefACTSMask    : Slv6Array (NARRAYS_BSA-1 downto 0);
     bsadefSeqSel      : Slv5Array (NARRAYS_BSA-1 downto 0);
     bsadefSeqBit      : Slv4Array (NARRAYS_BSA-1 downto 0);
     bsadefDestMode    : Slv2Array (NARRAYS_BSA-1 downto 0);
     bsadefDestInclM   : Slv16Array(NARRAYS_BSA-1 downto 0);
     bsadefDestExclM   : Slv16Array(NARRAYS_BSA-1 downto 0);
     bsadefNToAvg      : Slv13Array(NARRAYS_BSA-1 downto 0);
     bsadefAvgToWr     : Slv16Array(NARRAYS_BSA-1 downto 0);
     bsadefMaxSevr     : Slv2Array (NARRAYS_BSA-1 downto 0);
     bsaComplete       : slv(63 downto 0);
     bsaCompleteQ      : sl;
     countUpdate       : sl;
     FixedRateDivisors : Slv20Array(9 downto 0);
     ACRateDivisors    : Slv8Array(5 downto 0);
     config            : TPGConfigType;
     edefConfig        : TPGMiniEdefConfigType;
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
     bsadefRateMode    => (others=>(others=>'0')),
     bsadefFixedRate   => (others=>(others=>'0')),
     bsadefACRate      => (others=>(others=>'0')),
     bsadefACTSMask    => (others=>(others=>'1')),
     bsadefSeqSel      => (others=>(others=>'0')),
     bsadefSeqBit      => (others=>(others=>'0')),
     bsadefDestMode    => (others=>(others=>'0')),
     bsadefDestInclM   => (others=>(others=>'1')),
     bsadefDestExclM   => (others=>(others=>'1')),
     bsadefNToAvg      => (others=>toSlv(1,13)),
     bsadefAvgToWr     => (others=>toSlv(100,16)),
     bsadefMaxSevr     => (others=>(others=>'1')),
     bsaComplete       => (others=>'0'),
     bsaCompleteQ      => '0',
     countUpdate       => '0',
     FixedRateDivisors => TPG_CONFIG_INIT_C.FixedRateDivisors,
     ACRateDivisors    => TPG_CONFIG_INIT_C.ACRateDivisors,
     config            => TPG_CONFIG_INIT_C,
     edefConfig        => TPG_MINI_EDEF_CONFIG_INIT_C,
     txReset           => '0',
     txLoopback        => "000",
     txInhibit         => '0',
     rdData            => (others=>'0'),
     axiReadSlave      => AXI_LITE_READ_SLAVE_INIT_C,
     axiWriteSlave     => AXI_LITE_WRITE_SLAVE_INIT_C);

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

  -- tpgConfig.ACRateDivisors <= (x"01",
  --                              x"02",
  --                              x"06",
  --                              x"0C",
  --                              x"3C",
  --                              x"78");

   
begin

   assert NARRAYS_BSA < 33
     report "NARRAYS_BSA (" & integer'image(NARRAYS_BSA) & ") limit is 32 for TPGMini" severity failure;

   -------------------------------
   -- Configuration Register
   -------------------------------
   comb : process (axiReadMaster, axiRst, axiWriteMaster, irqActive, r, status) is
     variable v            : RegType;
     variable tmp          : sl;
     variable bsaClear     : slv(63 downto 0);
     variable bsainit      : slv(NARRAYS_BSA-1 downto 0);
     variable ep           : AxiLiteEndPointType;
   begin
      -- Latch the current value
      v := r;

      -- Reset strobing signals
      v.config.pulseIdWrEn   := '0';
      v.config.timeStampWrEn := '0';
      v.config.intervalRst   := '0';
      v.edefConfig.wrEn      := '0';
      v.txReset              := '0';
      bsaClear               := (others=>'0');

      axiSlaveWaitTxn(ep, axiWriteMaster, axiReadMaster, v.axiWriteSlave, v.axiReadSlave);

      axiSlaveRegister( ep, x"000", 1, v.config.txPolarity );
      axiSlaveRegister( ep, x"000", 2, v.txLoopback );
      axiSlaveRegister( ep, x"000", 5, v.txInhibit );
      axiSlaveRegister( ep, x"004", 0, v.config.baseDivisor );

      axiSlaveRegisterR( ep, x"008", 0, status.pulseId );
      axiSlaveRegisterR( ep, x"010", 0, status.timeStamp );
      
      for i in 0 to 9 loop
        axiSlaveRegister( ep, toSlv(24+4*i,12), 0, v.FixedRateDivisors(i) );
      end loop;
      axiWrDetect(ep, x"040",tmp);
      if tmp='1' then
        v.config.FixedRateDivisors := r.FixedRateDivisors;
      end if;

      axiSlaveRegisterR( ep, x"04C", 0, status.nbeamseq );
      axiSlaveRegisterR( ep, x"04C", 6, status.nexptseq );
      axiSlaveRegisterR( ep, x"04C",14, status.narraysbsa );
      axiSlaveRegisterR( ep, x"04C",22, status.seqaddrlen );
      axiSlaveRegisterR( ep, x"04C",26, status.nallowseq );
      axiSlaveRegister( ep, x"050", 0, bsaClear );
      axiSlaveRegisterR( ep, x"050", 0, r.bsaComplete );
      axiSlaveRegister( ep, x"058", 0, v.config.pulseId );
      axiSlaveRegister( ep, x"060", 0, v.config.timeStamp );

      axiSlaveRegister( ep, x"068", 0, v.txreset );
      axiSlaveRegister( ep, x"06C", 0, v.config.intervalRst );
      axiSlaveRegister( ep, x"070", 0, v.config.pulseIdWrEn );
      axiSlaveRegister( ep, x"074", 0, v.config.timeStampWrEn );
      
--      axiSlaveRegister( ep, x"078", 0, v.edefConfig ); -- need to complete this
      axiSlaveRegister( ep, x"07C", 0, v.edefConfig.wrEn );

      for i in 0 to 5 loop
        axiSlaveRegister( ep, toSlv(128+4*i,12), 0, v.ACRateDivisors(i) );
      end loop;
      axiWrDetect(ep, x"098",tmp);
      if tmp='1' then
        v.config.ACRateDivisors := r.ACRateDivisors;
      end if;

      bsainit := (others=>'0');
      axiSlaveRegister( ep, x"1FC", 0, bsainit);
      for i in 0 to NARRAYS_BSA-1 loop
        v.config.bsadefv(i).init := bsainit(i);
      end loop;

      for i in 0 to NARRAYS_BSA-1 loop
        axiSlaveRegister( ep, toSlv(512+16*i,12), 0, v.bsadefRateMode(i) );
        axiSlaveRegister( ep, toSlv(512+16*i,12), 2, v.bsadefFixedRate(i) );
        axiSlaveRegister( ep, toSlv(512+16*i,12), 6, v.bsadefACRate(i) );
        axiSlaveRegister( ep, toSlv(512+16*i,12), 9, v.bsadefACTSMask(i) );
        axiSlaveRegister( ep, toSlv(512+16*i,12),15, v.bsadefSeqSel(i) );
        axiSlaveRegister( ep, toSlv(512+16*i,12),20, v.bsadefSeqBit(i) );
        axiSlaveRegister( ep, toSlv(512+16*i,12),24, v.bsadefDestMode(i) );
        axiSlaveRegister( ep, toSlv(516+16*i,12), 0, v.bsadefDestInclM(i) );
        axiSlaveRegister( ep, toSlv(516+16*i,12),16, v.bsadefDestExclM(i) );
        
        axiSlaveRegister( ep, toSlv(520+16*i,12), 0, v.bsadefNToAvg(i) );
        axiSlaveRegister( ep, toSlv(520+16*i,12),14, v.bsadefMaxSevr(i) );
        axiSlaveRegister( ep, toSlv(520+16*i,12),16, v.bsadefAvgToWr(i) );
      end loop;

      for i in 0 to NARRAYS_BSA-1 loop
        axiSlaveRegisterR( ep, toSlv(1024+4*i,12), 0, status.bsastatus(i) );
      end loop;
      
      axiSlaveRegisterR( ep, x"500", 0, status.pllChanged );
      axiSlaveRegisterR( ep, x"504", 0, status.count186M );
      axiSlaveRegisterR( ep, x"508", 0, status.countSyncE );
      axiSlaveRegister ( ep, x"50C", 0, v.config.interval);
      axiSlaveRegisterR( ep, x"510", 0, status.countBRT );

      axiSlaveDefault(ep, v.axiWriteSlave, v.axiReadSlave);

      for i in 0 to NARRAYS_BSA-1 loop
        if (v.config.bsadefv(i).init ='1' and r.config.bsadefv(i).init='0') then
          v.config.bsadefv(i).rateSel(12 downto 11) := r.bsadefRateMode(i);
          case r.bsadefRateMode(i) is
            when "00"   => v.config.bsadefv(i).rateSel( 3 downto 0) := r.bsadefFixedRate(i);
            when "01"   => v.config.bsadefv(i).rateSel( 8 downto 3) := r.bsadefACTSMask (i);
                           v.config.bsadefv(i).rateSel( 2 downto 0) := r.bsadefACRate   (i);
            when others => v.config.bsadefv(i).rateSel(10 downto 6) := r.bsadefSeqSel   (i);
                           v.config.bsadefv(i).rateSel( 3 downto 0) := r.bsadefSeqBit   (i);
          end case;
          v.config.bsadefv(i).destSel(17 downto 16) := r.bsadefDestMode(i);
          case r.bsadefDestMode(i) is
            when "00"   => v.config.bsadefv(i).destSel(15 downto 0) := r.bsadefDestInclM(i);
            when "01"   => v.config.bsadefv(i).destSel(15 downto 0) := r.bsadefDestExclM(i);
            when others => null;
          end case;
          v.config.bsadefv(i).nToAvg  := r.bsadefNToAvg (i);
          v.config.bsadefv(i).avgToWr := r.bsadefAvgToWr(i);
          v.config.bsadefv(i).maxSevr := r.bsadefMaxSevr(i);
        end if;
      end loop;

      -- Misc. Mapping and Logic
      v.bsaComplete := (r.bsaComplete and not bsaClear) or status.bsaComplete;
      if allBits(r.bsaComplete,'0') then
        v.bsaCompleteQ := '0';
      else
        v.bsaCompleteQ := '1';
      end if;

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
      edefConfig      <= r.edefConfig;
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
