-------------------------------------------------------------------------------
-- Title         : DestDiagControl
-- Project       : LCLS-II Timing Pattern Generator
-------------------------------------------------------------------------------
-- File          : DestDiagControl.vhd
-- Author        : Matt Weaver, weaver@slac.stanford.edu
-- Created       : 03/11/2024
-------------------------------------------------------------------------------
-- Description:
-- Translation of BSA DEF to control bits in timing pattern
-------------------------------------------------------------------------------
-- This file is part of 'LCLS2 Timing Core'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'LCLS2 Timing Core', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------
-- Modification history:
-- 03/11/2024: created.
-------------------------------------------------------------------------------
LIBRARY ieee;
use work.all;
USE ieee.std_logic_1164.ALL;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;

library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;

entity DestDiagControl is
  generic ( TPD_G          : time    := 1 ns;
            DEFAULT_MASK_G : integer);
   port ( 
     clk                : in  sl;
     rst                : in  sl;
     enable             : in  sl;
     frame              : in  TimingMessageType;
     dataO              : out slv(3 downto 0);
     monReset           : in  sl;
     monCount           : out slv(127 downto 0);
     --
     axilReadMaster     : in  AxiLiteReadMasterType;
     axilReadSlave      : out AxiLiteReadSlaveType;
     axilWriteMaster    : in  AxiLiteWriteMasterType;
     axilWriteSlave     : out AxiLiteWriteSlaveType
     );
end DestDiagControl;

architecture rtl of DestDiagControl is

  type RegType is record
    intervalC : Slv20Array(3 downto 0);
    maskC     : slv(15 downto 0);
    interval  : Slv20Array(3 downto 0);
    enable    : sl;
    dataO     : slv(3 downto 0);
    monCount  : Slv32Array(3 downto 0);
    rslave    : AxiLiteReadSlaveType;
    wslave    : AxiLiteWriteSlaveType;
  end record;
  constant REG_INIT_C : RegType := (
    intervalC => (0 => toSlv(910000,20),
                  1 => toSlv( 91000,20),
                  2 => toSlv(  9100,20),
                  3 => toSlv(   910,20)),
    maskC     => toSlv(DEFAULT_MASK_G,16),
    interval  => (others=>(others=>'0')),
    enable    => '0',
    dataO     => (others=>'0'),
    monCount  => (others=>(others=>'0')),
    rslave    => AXI_LITE_READ_SLAVE_INIT_C,
    wslave    => AXI_LITE_WRITE_SLAVE_INIT_C );

  signal r   : RegType := REG_INIT_C;
  signal rin : RegType;
  
begin

  dataO          <= r.dataO;
  monCount       <= r.monCount(3) &
                    r.monCount(2) &
                    r.monCount(1) &
                    r.monCount(0);
  axilReadSlave  <= r.rslave;
  axilWriteSlave <= r.wslave;
  
  comb: process ( r, rst, monReset, enable, frame ) is
    variable v    : RegType;
    variable ep   : AxiLiteEndPointType;
    variable dest : integer;
  begin
    v := r;

    dest := conv_integer(frame.beamRequest(7 downto 4));

    v.enable := enable;
    for i in 0 to 3 loop
      if enable = '1' then
        v.dataO(i) := '0';
        if (r.interval(i)=0 and r.maskC(dest)='1' and frame.beamRequest(0)='1') then
          v.dataO(i) := '1';
        end if;
      end if;

      if r.enable = '1' then
        if r.dataO(i)='1' then
          v.interval(i) := r.intervalC(i)-1;
          v.monCount(i) := r.monCount(i)+1;
        elsif r.interval(i)/=0 then
          v.interval(i) := r.interval(i)-1;
        end if;
      end if;
    end loop;
        
    axiSlaveWaitTxn(ep, axilWriteMaster, axilReadMaster, v.wslave, v.rslave);

    -- Status Counters
    for i in 0 to 3 loop
      axiSlaveRegister (ep, toSlv(8*i+0,12), 0, v.intervalC(i));
      axiSlaveRegisterR(ep, toSlv(8*i+4,12), 0, r.monCount (i));
    end loop;
    axiSlaveRegister(ep, toSlv(32,12), 0, v.maskC);

    axiSlaveDefault(ep, v.wslave, v.rslave, AXI_RESP_DECERR_C);
    
    if rst = '1' then
      v := REG_INIT_C;
    end if;

    if monReset = '1' then
      v.monCount := (others=>(others=>'0'));
    end if;
    
    rin <= v;
  end process;

  seq: process ( clk ) is
  begin
    if rising_edge(clk) then
      r <= rin after TPD_G;
    end if;
  end process seq;
  
end rtl;
