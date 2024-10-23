-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- This file is part of 'L2SI Core'. It is subject to
-- the license terms in the LICENSE.txt file found in the top-level directory
-- of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'L2SI Core', including this file, may be
-- copied, modified, propagated, or distributed except according to the terms
-- contained in the LICENSE.txt file.
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

library l2si;
use l2si.XpmAppPkg.all;

entity XpmSeqXbar is
   generic (
      TPD_G            : time             := 1 ns;
      AXIL_BASEADDR_G  : slv(31 downto 0) := (others=>'0');
      AXIL_ASYNC_G     : boolean          := true;
      NUM_SEQ_G        : integer          := 1 );
   port (
      -- AXI-Lite Interface (on axiClk domain)
      axiClk          : in  sl;
      axiRst          : in  sl;
      axiReadMaster   : in  AxiLiteReadMasterType;
      axiReadSlave    : out AxiLiteReadSlaveType;
      axiWriteMaster  : in  AxiLiteWriteMasterType;
      axiWriteSlave   : out AxiLiteWriteSlaveType;
      -- Configuration/Status (on clk domain)
      clk             : in  sl;
      rst             : in  sl;
      status          : in  XpmSeqStatusArray(NUM_SEQ_G-1 downto 0);
      gconfig         : out XpmSeqGConfigType;
      config          : out XpmSeqConfigArray(NUM_SEQ_G-1 downto 0) );
end XpmSeqXbar;

architecture xbar of XpmSeqXbar is

   constant SEQSTATE_INDEX_C  : natural := 0;
   constant SEQJUMP_INDEX_C   : natural := 1;
   constant SEQMEM_INDEX_C    : natural := 2;
   constant NUM_AXI_MASTERS_C : natural := 3;

   constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(0 to 2) := (
      0                  => (baseAddr     => X"00000000" + AXIL_BASEADDR_G,
                             addrBits     => 14,
                             connectivity => X"FFFF"),
      1                  => (baseAddr     => X"00004000" + AXIL_BASEADDR_G,
                             addrBits     => 14,
                             connectivity => X"FFFF"),
      2                  => (baseAddr     => X"00010000" + AXIL_BASEADDR_G,
                             addrBits     => 16,
                             connectivity => X"FFFF"));

   signal mAxilWriteMasters : AxiLiteWriteMasterArray(NUM_AXI_MASTERS_C-1 downto 0);
   signal mAxilWriteSlaves  : AxiLiteWriteSlaveArray (NUM_AXI_MASTERS_C-1 downto 0);
   signal mAxilReadMasters  : AxiLiteReadMasterArray (NUM_AXI_MASTERS_C-1 downto 0);
   signal mAxilReadSlaves   : AxiLiteReadSlaveArray  (NUM_AXI_MASTERS_C-1 downto 0);

   signal syncWriteMaster : AxiLiteWriteMasterType;
   signal syncWriteSlave  : AxiLiteWriteSlaveType;
   signal syncReadMaster  : AxiLiteReadMasterType;
   signal syncReadSlave   : AxiLiteReadSlaveType;

   signal statusS    : XpmSeqStatusArray(NUM_SEQ_G-1 downto 0);
   signal statusSlv  : slv(NUM_SEQ_G*XPM_SEQ_STATUS_BITS_C-1 downto 0);
   signal statusSlvS : slv(NUM_SEQ_G*XPM_SEQ_STATUS_BITS_C-1 downto 0);

   signal gConfigS : XpmSeqGConfigType;
   signal mConfig0,mConfig1,mConfig2    : XpmSeqConfigArray(NUM_SEQ_G-1 downto 0);
   signal mConfigS0,mConfigS1,mConfigS2 : XpmSeqConfigArray(NUM_SEQ_G-1 downto 0);

   signal gConfigSlv,gConfigSlvS : slv(SEQADDRLEN+31 downto 0);
   type XpmSeqConfigSlvArray is array (natural range <>) of slv(NUM_SEQ_G*XPM_SEQ_CONFIG_BITS_C-1 downto 0);
   signal mConfigSlv  : XpmSeqConfigSlvArray(NUM_AXI_MASTERS_C-1 downto 0);
   signal mConfigSlvS : XpmSeqConfigSlvArray(NUM_AXI_MASTERS_C-1 downto 0);

   signal regclk : sl;
   signal regrst : sl;
begin

   --------------------------
   -- AXI-Lite: Crossbar Core
   --------------------------
   U_XBAR : entity surf.AxiLiteCrossbar
      generic map (
         TPD_G              => TPD_G,
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => NUM_AXI_MASTERS_C,
         MASTERS_CONFIG_G   => AXI_CROSSBAR_MASTERS_CONFIG_C)
      port map (
         axiClk              => regclk,
         axiClkRst           => regrst,
         sAxiWriteMasters(0) => syncWriteMaster,
         sAxiWriteSlaves(0)  => syncWriteSlave,
         sAxiReadMasters(0)  => syncReadMaster,
         sAxiReadSlaves(0)   => syncReadSlave,
         mAxiWriteMasters    => mAxilWriteMasters,
         mAxiWriteSlaves     => mAxilWriteSlaves,
         mAxiReadMasters     => mAxilReadMasters,
         mAxiReadSlaves      => mAxilReadSlaves);

   U_SeqJumpReg : entity l2si.XpmSeqJumpReg
      generic map (
         NUM_SEQ_G => NUM_SEQ_G )
      port map (
         axiReadMaster  => mAxilReadMasters (SEQJUMP_INDEX_C),
         axiReadSlave   => mAxilReadSlaves  (SEQJUMP_INDEX_C),
         axiWriteMaster => mAxilWriteMasters(SEQJUMP_INDEX_C),
         axiWriteSlave  => mAxilWriteSlaves (SEQJUMP_INDEX_C),
         status         => statusS,
         config         => mConfigS0,
         axiClk         => regclk,
         axiRst         => regrst);

   U_SeqStateReg : entity l2si.XpmSeqStateReg
      generic map (
         NUM_SEQ_G => NUM_SEQ_G )
      port map (
         axiReadMaster  => mAxilReadMasters (SEQSTATE_INDEX_C),
         axiReadSlave   => mAxilReadSlaves  (SEQSTATE_INDEX_C),
         axiWriteMaster => mAxilWriteMasters(SEQSTATE_INDEX_C),
         axiWriteSlave  => mAxilWriteSlaves (SEQSTATE_INDEX_C),
         status         => statusS,
         config         => mConfigS1,
         axiClk         => regclk,
         axiRst         => regrst);

   U_SeqMemReg : entity l2si.XpmSeqMemReg
      generic map (
         NUM_SEQ_G   => NUM_SEQ_G,
         ADDR_BITS_G => 16 )
      port map (
         axiReadMaster  => mAxilReadMasters (SEQMEM_INDEX_C),
         axiReadSlave   => mAxilReadSlaves  (SEQMEM_INDEX_C),
         axiWriteMaster => mAxilWriteMasters(SEQMEM_INDEX_C),
         axiWriteSlave  => mAxilWriteSlaves (SEQMEM_INDEX_C),
         status         => statusS,
         gconfig        => gConfigS,
         config         => mConfigS2,
         axiClk         => regclk,
         axiRst         => regrst);

   GEN_ASYNC : if AXIL_ASYNC_G generate
     regclk     <= clk;
     regrst     <= rst;
     statusS    <= status;
     mConfig0   <= mConfigS0;
     mConfig1   <= mConfigS1;
     mConfig2   <= mConfigS2;

     U_AxiLiteAsync : entity surf.AxiLiteAsync
       generic map (
         TPD_G        => TPD_G )
       port map (
         sAxiClk         => axiClk,
         sAxiClkRst      => axiRst,
         sAxiReadMaster  => axiReadMaster,
         sAxiReadSlave   => axiReadSlave,
         sAxiWriteMaster => axiWriteMaster,
         sAxiWriteSlave  => axiWriteSlave,
         mAxiClk         => clk,
         mAxiClkRst      => rst,
         mAxiReadMaster  => syncReadMaster,
         mAxiReadSlave   => syncReadSlave,
         mAxiWriteMaster => syncWriteMaster,
         mAxiWriteSlave  => syncWriteSlave );
   end generate;

   GEN_SYNC : if not AXIL_ASYNC_G generate
     regclk     <= axiClk;
     regrst     <= axiRst;

     syncReadMaster  <= axiReadMaster;
     axiReadSlave    <= syncReadSlave;
     syncWriteMaster <= axiWriteMaster;
     axiWriteSlave   <= syncWriteSlave;

     statusSlv  <= toSlv(status);
     statusS    <= toXpmSeqStatusArray(statusSlvS, NUM_SEQ_G);

     U_StatusSync : entity surf.SynchronizerVector
       generic map (
         WIDTH_G => statusSlv'length)
       port map (
         clk     => regclk,
         dataIn  => statusSlv,
         dataOut => statusSlvS);

     gconfig  <= toXpmSeqGConfigType(gConfigSlv);
     mConfig0 <= toXpmSeqConfigArray(mConfigSlv(0),NUM_SEQ_G);
     mConfig1 <= toXpmSeqConfigArray(mConfigSlv(1),NUM_SEQ_G);
     mConfig2 <= toXpmSeqConfigArray(mConfigSlv(2),NUM_SEQ_G);
     gConfigSlvS    <= toSlv(gConfigS);
     mConfigSlvS(0) <= toSlv(mConfigS0);
     mConfigSlvS(1) <= toSlv(mConfigS1);
     mConfigSlvS(2) <= toSlv(mConfigS2);
     
     U_ConfigSyncG : entity surf.SynchronizerVector
       generic map (
         WIDTH_G => gConfigSlv'length)
       port map (
         clk     => clk,
         dataIn  => gConfigSlvS,
         dataOut => gConfigSlv );
     GEN_CONFIG_SYNC : for i in 0 to NUM_AXI_MASTERS_C-1 generate
       U_ConfigSync : entity surf.SynchronizerVector
         generic map (
           WIDTH_G => mConfigSlvS(i)'length)
         port map (
           clk     => clk,
           dataIn  => mConfigSlvS(i),
           dataOut => mConfigSlv (i) );
     end generate GEN_CONFIG_SYNC;
   end generate;

   -------------------------------
   -- Configuration Register
   -------------------------------
   comb : process (mConfig0,mConfig1,mConfig2) is
      variable v : XpmSeqConfigArray(NUM_SEQ_G-1 downto 0);
   begin
      for i in 0 to NUM_SEQ_G-1 loop
        v(i)               := mConfig2(i);
        v(i).seqJumpConfig := mConfig1(i).seqJumpConfig;
        v(i).seqEnable     := mConfig0(i).seqEnable;
        v(i).seqRestart    := mConfig0(i).seqRestart;
        config(i)          <= v(i);
      end loop;
   end process comb;

end xbar;
