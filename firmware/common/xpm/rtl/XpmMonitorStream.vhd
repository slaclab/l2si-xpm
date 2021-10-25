-----------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmMonitorStream.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2021-10-15
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Software programmable register interface
-------------------------------------------------------------------------------
-- This file is part of 'LCLS2 XPM Core'.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'LCLS2 XPM Core', including this file,
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
use surf.AxiStreamPkg.all;
use surf.SsiPkg.all;
use surf.EthMacPkg.all;

--library amc_carrier_core;
--use amc_carrier_core.AmcCarrierPkg.all;  -- ETH_AXIS_CONFIG_C

library l2si_core;
use l2si_core.XpmPkg.all;

entity XpmMonitorStream is
   port (
      axilClk         : in  sl;
      axilRst         : in  sl;
      enable          : in  sl;
      period          : in  slv(26 downto 0);
      pllCount        : in  SlVectorArray(3 downto 0, 2 downto 0);
      pllStat         : in  slv          (3 downto 0);
      monClkRate      : in  Slv32Array   (3 downto 0);
      status          : in  XpmStatusType;
      staClk          : in  sl;
      -- Status
      busy            : out sl;
      count           : out slv(26 downto 0);
      id              : out slv(31 downto 0);
      index           : out slv( 9 downto 0);
      -- Application Debug Interface (sysclk domain)
      obMonitorMaster : out AxiStreamMasterType;
      obMonitorSlave  : in  AxiStreamSlaveType );
end XpmMonitorStream;

architecture rtl of XpmMonitorStream is

   signal sL0Stats : Slv200Array(XPM_PARTITIONS_C-1 downto 0);

   constant TDATA_SIZE_C      : integer := EMAC_AXIS_CONFIG_C.TDATA_BYTES_C*8;
   constant XPM_STATUS_BITS_C : integer := 8*(4 + 14*12 + 8*382 + 16);
   constant LAST_WORD_C       : integer := (XPM_STATUS_BITS_C-1) / TDATA_SIZE_C;
   
   function toSlv(packetId : slv(15 downto 0);
                  s        : XpmStatusType;
                  sL0Stats : Slv200Array(XPM_PARTITIONS_C-1 downto 0);      
                  pllCount : SlVectorArray(3 downto 0, 2 downto 0);
                  pllStat  : slv(3 downto 0);
                  monClkR  : Slv32Array(3 downto 0) ) return slv is
     variable v : slv(XPM_STATUS_BITS_C-1 downto 0) := (others=>'0');
     variable i : integer := 0;
   begin
     assignSlv(i, v, x"0002"); -- 2B
     assignSlv(i, v, packetId); -- 2B
     -- dsLinkStatus
     for j in 0 to 13 loop
       assignSlv(i, v, toSlv(s.dsLink(j)));                -- 86b
       assignSlv(i, v, toSlv(0,10));                       -- 10b
     end loop; -- 14*12B
     for j in 0 to XPM_PARTITIONS_C-1 loop
       for k in 0 to 31 loop
         assignSlv(i, v, s.partition(j).inhibit.evcounts(k)); -- 32*32b -- regclk
         assignSlv(i, v, s.partition(j).inhibit.tmcounts(k)); -- 32*32b -- regclk
       end loop;
       assignSlv(i, v, sL0Stats(j) ); -- 200b
       assignSlv(i, v, x"ee"); -- 1B
     end loop; -- 8*382B
     for j in 0 to 3 loop
       assignSlv(i, v, pllStat(j));
       assignSlv(i, v, muxSlVectorArray(pllCount,j));
     end loop; -- 2B
     for j in 0 to 3 loop
       assignSlv(i, v, monClkR(j));
     end loop; --16B
     return v; --
   end function toSlv;
   
   type RegType is record
      busy           : sl;
      count          : slv(period'range);
      id             : slv(15 downto 0);
      index          : integer range 0 to LAST_WORD_C;
      dataL          : sl;
      data           : slv(XPM_STATUS_BITS_C-1 downto 0);
      master         : AxiStreamMasterType;
   end record RegType;

   constant REG_INIT_C : RegType := (
      busy           => '0',
      count          => (others => '0'),
      id             => (others => '0'),
      index          => 0,
      dataL          => '0',
      data           => (others => '0'),
      master         => AxiStreamMasterInit(EMAC_AXIS_CONFIG_C) );

   signal r    : RegType := REG_INIT_C;
   signal r_in : RegType;

   signal dataL : sl;
   
begin

  busy  <= r.busy;
  count <= r.count;
  id    <= x"0000" & r.id;
  index <= toSlv(r.index,10);
  obMonitorMaster <= r.master;

  U_DATAL : BUFG
    port map (
      I => r.dataL,
      O => dataL );
  
  GEN_L0 : for i in 0 to XPM_PARTITIONS_C-1 generate
    SYNC_L0 : entity surf.SynchronizerFifo
      generic map ( DATA_WIDTH_G => 200 )
      port map ( rst                 => axilRst,
                 wr_clk              => staClk,
                 din( 39 downto   0) => status.partition(i).l0Select.enabled,
                 din( 79 downto  40) => status.partition(i).l0Select.inhibited,
                 din(119 downto  80) => status.partition(i).l0Select.num,
                 din(159 downto 120) => status.partition(i).l0Select.numInh,
                 din(199 downto 160) => status.partition(i).l0Select.numAcc,
                 rd_clk              => axilClk,
                 dout                => sL0Stats(i) );
  end generate GEN_L0;
  
  comb : process ( axilRst, r, enable, period, status, sL0Stats,
                   pllCount, pllStat, monClkRate, dataL,
                   obMonitorSlave ) is
    variable v  : RegType;
  begin
    v := r;

    v.dataL := '0';
    
    if obMonitorSlave.tReady = '1' then
      v.master.tValid := '0';
    end if;

    if enable = '1' then
      v.count := r.count + 1;
      if r.busy = '1' then
        if v.master.tValid = '0' then
          v.master.tValid := '1';
          v.master.tLast  := '0';
          v.master.tData(TDATA_SIZE_C-1 downto 0) := r.data(TDATA_SIZE_C-1 downto 0);
          v.data := toSlv(0,TDATA_SIZE_C) & r.data(r.data'left downto TDATA_SIZE_C);

          if r.index = 0 then
            ssiSetUserSof (EMAC_AXIS_CONFIG_C, v.master, '1');
          else
            ssiSetUserSof (EMAC_AXIS_CONFIG_C, v.master, '0');
          end if;
          ssiSetUserEofe(EMAC_AXIS_CONFIG_C, v.master, '0');
          
          if r.index = LAST_WORD_C then
            v.master.tLast := '1';
            v.busy         := '0';
          else
            v.index := r.index + 1;
          end if;
        end if;
      end if;

      if r.count = period then
        v.count := (others=>'0');
        v.id    := r.id + 1;
        -- latch the whole thing here
        if r.busy = '0' then
          v.dataL := '1';
        end if;
      end if;
    else
      v.count := (others=>'0');
    end if;

    if dataL = '1' then
      v.data := toSlv(r.id, status, sL0Stats, pllCount, pllStat, monClkRate);
      v.index := 0;
      v.busy  := '1';
    end if;
    
    if axilRst = '1' then
      v := REG_INIT_C;
    end if;

    r_in <= v;

  end process comb;

  seq : process ( axilClk) is
  begin
    if rising_edge(axilClk) then
      r <= r_in;
    end if;
  end process seq;
  
end rtl;
