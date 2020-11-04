-----------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmReg.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2020-10-24
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

library amc_carrier_core;
use amc_carrier_core.AmcCarrierPkg.all;  -- ETH_AXIS_CONFIG_C

library l2si_core;
use l2si_core.XpmPkg.all;

library l2si;

entity XpmReg is
   generic(
      TPD_G               : time    := 1 ns;
      NUM_DS_LINKS_G      : integer;
      NUM_BP_LINKS_G      : integer;
      US_RX_ENABLE_INIT_G : boolean := true;
      CU_RX_ENABLE_INIT_G : boolean := false);
   port (
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilUpdate      : out slv(XPM_PARTITIONS_C-1 downto 0);
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      groupLinkClear  : in  slv(XPM_PARTITIONS_C-1 downto 0);
      -- Application Debug Interface (sysclk domain)
      ibDebugMaster   : in  AxiStreamMasterType;
      ibDebugSlave    : out AxiStreamSlaveType;
      obDebugMaster   : out AxiStreamMasterType;
      obDebugSlave    : in  AxiStreamSlaveType;
      obMonitorMaster : out AxiStreamMasterType;
      obMonitorSlave  : in  AxiStreamSlaveType;
      --
      staClk          : in  sl;
      pllStatus       : in  XpmPllStatusArray(XPM_NUM_AMCS_C-1 downto 0);
      status          : in  XpmStatusType;
      monClk          : in  slv(3 downto 0) := (others => '0');
      config          : out XpmConfigType;
      usRxEnable      : out sl;
      cuRxEnable      : out sl;
      dbgChan         : out slv(4 downto 0));
end XpmReg;

architecture rtl of XpmReg is

   type StateType is (IDLE_S, READING_S);

   type StepType is record
      enable   : sl;
      numL0Acc : slv(31 downto 0);
      groups   : slv(XPM_PARTITIONS_C-1 downto 0);
   end record;

   constant STEP_INIT_C : StepType := (
      enable   => '0',
      numL0Acc => (others=>'0'),
      groups   => (others=>'0') );

   type StepArray is array (natural range<>) of StepType;
   
   type RegType is record
      state          : StateType;
      tagSlave       : AxiStreamSlaveType;
      load           : sl;
      config         : XpmConfigType;
      partition      : slv(3 downto 0);
      link           : slv(4 downto 0);
      amc            : slv(0 downto 0);
      inhibit        : slv(1 downto 0);
      linkCfg        : XpmLinkConfigType;
      linkStat       : XpmLinkStatusType;
      partitionCfg   : XpmPartitionConfigType;
      partitionStat  : XpmPartitionStatusType;
      pllCfg         : XpmPllConfigType;
      inhibitCfg     : XpmInhibitConfigType;
      axilReadSlave  : AxiLiteReadSlaveType;
      axilWriteSlave : AxiLiteWriteSlaveType;
      axilRdEn       : slv(XPM_PARTITIONS_C-1 downto 0);
      linkDebug      : slv(4 downto 0);
      anaWrCount     : Slv32Array(XPM_PARTITIONS_C-1 downto 0);
      usRxEnable     : sl;
      cuRxEnable     : sl;
      step           : StepArray(XPM_PARTITIONS_C-1 downto 0);
      stepMaster     : AxiStreamMasterType;
      monStreamEnable: sl;
      monStreamPeriod: slv(26 downto 0);
   end record RegType;

   constant REG_INIT_C : RegType := (
      state          => IDLE_S,
      tagSlave       => AXI_STREAM_SLAVE_INIT_C,
      load           => '1',
      config         => XPM_CONFIG_INIT_C,
      partition      => (others => '0'),
      link           => (others => '0'),
      amc            => (others => '0'),
      inhibit        => (others => '0'),
      linkCfg        => XPM_LINK_CONFIG_INIT_C,
      linkStat       => XPM_LINK_STATUS_INIT_C,
      partitionCfg   => XPM_PARTITION_CONFIG_INIT_C,
      partitionStat  => XPM_PARTITION_STATUS_INIT_C,
      pllCfg         => XPM_PLL_CONFIG_INIT_C,
      inhibitCfg     => XPM_INHIBIT_CONFIG_INIT_C,
      axilReadSlave  => AXI_LITE_READ_SLAVE_INIT_C,
      axilWriteSlave => AXI_LITE_WRITE_SLAVE_INIT_C,
      axilRdEn       => (others => '1'),
      linkDebug      => (others => '0'),
      anaWrCount     => (others => (others => '0')),
      usRxEnable     => toSl(US_RX_ENABLE_INIT_G),
      cuRxEnable     => toSl(CU_RX_ENABLE_INIT_G),
      step           => (others=>STEP_INIT_C),
      stepMaster     => AxiStreamMasterInit(EMAC_AXIS_CONFIG_C),
      monStreamEnable=> '0',
      monStreamPeriod=> toSlv(125000000,27) );

   signal r    : RegType := REG_INIT_C;
   signal r_in : RegType;

   type QRegType is record
      cnt       : slv(19 downto 0);
      staUpdate : sl;
      stepDone  : slv(XPM_PARTITIONS_C-1 downto 0);
   end record;

   constant QREG_INIT_C : QRegType := (
      cnt       => (others=>'0'),
      staUpdate => '0',
      stepDone  => (others=>'0') );
   
   signal q    : QRegType := QREG_INIT_C;
   signal q_in : QRegType;

   constant TAG_AXIS_CONFIG_C : AxiStreamConfigType := ssiAxiStreamConfig(8);
   signal tagMaster           : AxiStreamMasterType;

   signal pll_stat : slv(2*XPM_NUM_AMCS_C-1 downto 0);
   signal pllStat  : slv(2*XPM_NUM_AMCS_C-1 downto 0);
   signal pllCount : SlVectorArray(2*XPM_NUM_AMCS_C-1 downto 0, 2 downto 0);

   signal s        : XpmStatusType;
   signal linkStat : XpmLinkStatusType;
   signal staRst   : sl;
   
   signal monClkRate : Slv32Array(3 downto 0);
   signal monClkLock : slv (3 downto 0);
   signal monClkSlow : slv (3 downto 0);
   signal monClkFast : slv (3 downto 0);

   constant DEBUG_C : boolean := true;

   signal p0InhCh  : sl;
   signal p0InhErr : sl;
   signal pInhV    : slv(XPM_PARTITIONS_C-1 downto 0);

   signal step     : StepArray(XPM_PARTITIONS_C-1 downto 0);
   signal stepDone : slv      (XPM_PARTITIONS_C-1 downto 0);

   signal monBusy  : sl;
   signal monCount : slv(26 downto 0);
   signal monId    : slv(31 downto 0);
   signal monIndex : slv( 9 downto 0);
   
   component ila_0
      port (
         clk    : in sl;
         probe0 : in slv(255 downto 0));
   end component;

begin

   GEN_DBUG : if DEBUG_C generate
      U_ILA : ila_0
         port map (
            clk                   => axilClk,
            probe0(0)             => pInhV(0),
            probe0(1)             => p0InhCh,
            probe0(2)             => p0InhErr,
            probe0(66 downto 3)   => resize(s.partition(0).l0Select.inhibited, 64),
            probe0(70 downto 67)  => pll_stat,
            probe0(74 downto 71)  => pllStat,
            probe0(255 downto 75) => (others => '0'));

      process (axilClk) is
         variable p0Inh, p0Inhi : slv(15 downto 0);
      begin
         if rising_edge(axilClk) then
            p0Inhi := s.partition(0).l0Select.inhibited(p0Inh'range);
            if p0Inh /= p0Inhi then
               p0InhCh <= '1' after TPD_G;
            else
               p0InhCh <= '0' after TPD_G;
            end if;
            if p0Inh > p0Inhi then
               p0InhErr <= '1' after TPD_G;
            else
               p0InhErr <= '0' after TPD_G;
            end if;
            p0Inh := p0Inhi;
         end if;
      end process;
   end generate;

   dbgChan        <= r.linkDebug(dbgChan'range);
   config         <= r.config;
   axilReadSlave  <= r.axilReadSlave;
   axilWriteSlave <= r.axilWriteSlave;
   axilUpdate     <= r.axilRdEn;
   usRxEnable     <= r.usRxEnable;
   cuRxEnable     <= r.cuRxEnable;
   obDebugMaster  <= r.stepMaster;
   
   GEN_MONCLK : for i in 0 to 3 generate
      U_SYNC : entity surf.SyncClockFreq
         generic map (
            TPD_G             => TPD_G,
            REF_CLK_FREQ_G    => 125.00E+6,
            CLK_LOWER_LIMIT_G => 95.0E+6,
            CLK_UPPER_LIMIT_G => 186.0E+6)
         port map (
            freqOut     => monClkRate(i),
            freqUpdated => open,
            locked      => monClkLock(i),
            tooFast     => monClkFast(i),
            tooSlow     => monClkSlow(i),
            clkIn       => monClk(i),
            locClk      => axilClk,
            refClk      => axilClk);
   end generate;

   --
   --  Still need to cross clock-domains for register readout of:
   --    link status (32 links)
   --    partition inhibit counts (from 32 links for each partition)
   --

   GEN_BP : for i in 0 to NUM_BP_LINKS_G generate
      U_LinkUp : entity surf.Synchronizer
         generic map (
            TPD_G => TPD_G)
         port map (
            clk     => axilClk,
            dataIn  => status.bpLink(i).linkUp,
            dataOut => s.bpLink(i).linkUp);

      U_IbRecv : entity surf.SynchronizerVector
         generic map (
            TPD_G   => TPD_G,
            WIDTH_G => 32)
         port map (
            clk     => axilClk,
            dataIn  => status.bpLink(i).ibRecv,
            dataOut => s.bpLink(i).ibRecv);

      U_RxLate : entity surf.SynchronizerVector
         generic map (
            TPD_G   => TPD_G,
            WIDTH_G => 16)
         port map (
            clk     => axilClk,
            dataIn  => status.bpLink(i).rxLate,
            dataOut => s.bpLink(i).rxLate);
   end generate;

   GEN_PART : for i in 0 to XPM_PARTITIONS_C-1 generate
      U_Sync64_ena : entity surf.SynchronizerFifo
         generic map (
            TPD_G        => TPD_G,
            DATA_WIDTH_G => XPM_LCTR_DEPTH_C)
         port map (
            wr_clk => staClk,
            wr_en  => q.staUpdate,
            rd_clk => axilClk,
            rd_en  => r.axilRdEn(i),
            din    => status.partition(i).l0Select.enabled,
            dout   => s.partition(i).l0Select.enabled);

      U_Sync64_inh : entity surf.SynchronizerFifo
         generic map (
            TPD_G        => TPD_G,
            DATA_WIDTH_G => XPM_LCTR_DEPTH_C)
         port map (
            wr_clk => staClk,
            wr_en  => q.staUpdate,
            rd_clk => axilClk,
            rd_en  => r.axilRdEn(i),
            din    => status.partition(i).l0Select.inhibited,
            valid  => pInhV(i),
            dout   => s.partition(i).l0Select.inhibited);

      U_Sync64_num : entity surf.SynchronizerFifo
         generic map (
            TPD_G        => TPD_G,
            DATA_WIDTH_G => XPM_LCTR_DEPTH_C)
         port map (
            wr_clk => staClk,
            wr_en  => q.staUpdate,
            rd_clk => axilClk,
            rd_en  => r.axilRdEn(i),
            din    => status.partition(i).l0Select.num,
            dout   => s.partition(i).l0Select.num);

      U_Sync64_nin : entity surf.SynchronizerFifo
         generic map (
            TPD_G        => TPD_G,
            DATA_WIDTH_G => XPM_LCTR_DEPTH_C)
         port map (
            wr_clk => staClk,
            wr_en  => q.staUpdate,
            rd_clk => axilClk,
            rd_en  => r.axilRdEn(i),
            din    => status.partition(i).l0Select.numInh,
            dout   => s.partition(i).l0Select.numInh);

      U_Sync64_nac : entity surf.SynchronizerFifo
         generic map (
            TPD_G        => TPD_G,
            DATA_WIDTH_G => XPM_LCTR_DEPTH_C)
         port map (
            wr_clk => staClk,
            wr_en  => q.staUpdate,
            rd_clk => axilClk,
            rd_en  => r.axilRdEn(i),
            din    => status.partition(i).l0Select.numAcc,
            dout   => s.partition(i).l0Select.numAcc);
   end generate;

   GEN_LOL : for i in 0 to XPM_NUM_AMCS_C-1 generate
      pll_stat(2*i+0) <= pllStatus(i).los;
      pll_stat(2*i+1) <= pllStatus(i).lol;
   end generate;

   U_StatLol : entity surf.SyncStatusVector
      generic map (
         TPD_G        => TPD_G,
         COMMON_CLK_G => true,
         WIDTH_G      => 2*XPM_NUM_AMCS_C,
         CNT_WIDTH_G  => 3)
      port map (
         statusIn     => pll_stat,
         statusOut    => pllStat,
         cntRstIn     => '0',
         rollOverEnIn => (others => '1'),
         cntOut       => pllCount,
         wrClk        => axilClk,
         rdClk        => axilClk);

   U_AnalysisFifo : entity surf.AxiStreamFifo
      generic map (
         TPD_G               => TPD_G,
         SLAVE_AXI_CONFIG_G  => AXIS_8BYTE_CONFIG_C,
         MASTER_AXI_CONFIG_G => TAG_AXIS_CONFIG_C)
      port map (
         sAxisClk    => axilClk,
         sAxisRst    => axilRst,
         sAxisMaster => ibDebugMaster,
         sAxisSlave  => ibDebugSlave,
         mAxisClk    => axilClk,
         mAxisRst    => axilRst,
         mAxisMaster => tagMaster,
         mAxisSlave  => r_in.tagSlave);

   comb : process (r, axilReadMaster, axilWriteMaster, tagMaster, status, s, axilRst,
                   pllCount, pllStat, groupLinkClear, stepDone, obDebugSlave,
                   monClkRate, monClkLock, monClkFast, monClkSlow,
                   monCount, monIndex, monBusy, monId) is
      variable v              : RegType;
      variable axilEp         : AxiLiteEndpointType;
      variable ip             : integer;
      variable il             : integer;
      variable ia             : integer;
      variable groupL0Reset   : slv(XPM_PARTITIONS_C-1 downto 0);
      variable groupL0Enable  : slv(XPM_PARTITIONS_C-1 downto 0);
      variable groupL0Disable : slv(XPM_PARTITIONS_C-1 downto 0);
      variable groupMsgInsert : slv(XPM_PARTITIONS_C-1 downto 0);
      variable paddr          : slv(XPM_PARTITION_ADDR_LENGTH_C-1 downto 0);
   begin
      v                             := r;
      -- reset strobing signals
      v.axilReadSlave.rdata         := (others => '0');
      v.tagSlave.tReady             := '1';
      v.partitionCfg.message.insert := '0';

      ip := conv_integer(r.partition);
      il := conv_integer(r.link(3 downto 0));
      ia := conv_integer(r.amc);

      if r.load = '1' then
         if r.link(4) = '0' then
            v.linkCfg := r.config.dsLink(il);
         else
            v.linkCfg := r.config.bpLink(il);
         end if;
         v.partitionCfg := r.config.partition(ip);
         v.pllCfg       := r.config.pll (ia);
      else
         if r.link(4) = '0' then
            v.config.dsLink (il) := r.linkCfg;
         else
            v.config.bpLink (il) := r.linkCfg;
         end if;
         v.config.partition(ip) := r.partitionCfg;
         v.config.pll (ia)      := r.pllCfg;
      end if;

      --  strobing signals only set by group registers
      for i in 0 to XPM_PARTITIONS_C-1 loop
         v.config.partition(i).l0Select.reset := '0';
         v.config.partition(i).message.insert := '0';
      end loop;
      
      if r.link(4) = '0' then
         v.linkStat := status.dsLink (il);  -- clock-domain?
      elsif r.link(3 downto 0) = toSlv(0, 4) then
         v.linkStat           := XPM_LINK_STATUS_INIT_C;
         v.linkStat.txReady   := s.bpLink (il).linkUp;
         v.linkStat.rxReady   := s.bplink (il).ibRecv(0);
         v.linkStat.rxErrCnts := s.bplink (il).rxErrs;
      elsif r.link(3) = '0' then
         v.linkStat           := XPM_LINK_STATUS_INIT_C;
         v.linkStat.rxReady   := s.bpLink (il).linkUp;
         v.linkStat.rxRcvCnts := s.bpLink (il).ibRecv;
         v.linkStat.rxErrCnts := s.bpLink (il).rxErrs;
      else
         v.linkStat           := XPM_LINK_STATUS_INIT_C;
         v.linkStat.rxErrCnts := s.bpLink (conv_integer(r.link(2 downto 0))).rxLate;
      end if;
      v.partitionStat := status.partition(ip);

      -- Determine the transaction type
      axiSlaveWaitTxn(axilEp, axilWriteMaster, axilReadMaster, v.axilWriteSlave, v.axilReadSlave);
      v.axilReadSlave.rdata := (others => '0');

      -- Read/write to the configuration registers
      -- Read only from status registers


      paddr := status.paddr;
      axiSlaveRegister(axilEp, X"000", 0, paddr);

      axiSlaveRegister(axilEp, X"004", 0, v.partition);
      axiSlaveRegister(axilEp, X"004", 4, v.link);
      axiSlaveRegister(axilEp, X"004", 10, v.linkDebug);
      axiSlaveRegister(axilEp, X"004", 16, v.amc);
      axiSlaveRegister(axilEp, X"004", 20, v.inhibit);
--    axiSlaveRegister(axilEp, X"004", 24, v.config.tagStream);
--    axiSlaveRegister(axilEp, X"004", 25, v.usRxEnable);
--    axiSlaveRegister(axilEp, X"004", 26, v.cuRxEnable);
      axiSlaveRegisterR(axilEp, X"004", 25, toSl(US_RX_ENABLE_INIT_G));
      axiSlaveRegisterR(axilEp, X"004", 26, toSl(CU_RX_ENABLE_INIT_G));

      v.load := '0';

      axiWrDetect(axilEp, X"000", v.load);
      if (v.load = '1') then
         v.config.paddr := paddr;
      end if;

      v.load := '0';
      axiWrDetect(axilEp, X"004", v.load);

      axiSlaveRegister(axilEp, X"008", 0, v.linkCfg.groupMask);
--    axiSlaveRegister(axilEp, X"008",  9, v.linkCfg.rxTimeOut);
      axiSlaveRegister(axilEp, X"008", 18, v.linkCfg.txPllReset);
      axiSlaveRegister(axilEp, X"008", 19, v.linkCfg.rxPllReset);
--    axiSlaveRegister(axilEp, X"008", 24, v.linkCfg.trigsrc);
      axiSlaveRegister(axilEp, X"008", 28, v.linkCfg.loopback);
      axiSlaveRegister(axilEp, X"008", 29, v.linkCfg.txReset);
      axiSlaveRegister(axilEp, X"008", 30, v.linkCfg.rxReset);
      axiSlaveRegister(axilEp, X"008", 31, v.linkCfg.enable);

      axiSlaveRegisterR(axilEp, X"00C", 0, r.linkStat.rxErrCnts);
      axiSlaveRegisterR(axilEp, X"00C", 16, r.linkStat.txResetDone);
      axiSlaveRegisterR(axilEp, X"00C", 17, r.linkStat.txReady);
      axiSlaveRegisterR(axilEp, X"00C", 18, r.linkStat.rxResetDone);
      axiSlaveRegisterR(axilEp, X"00C", 19, r.linkStat.rxReady);
      axiSlaveRegisterR(axilEp, X"00C", 20, r.linkStat.rxIsXpm);

      axiSlaveRegisterR(axilEp, X"010", 0, r.linkStat.rxRcvCnts);

      axiSlaveRegister(axilEp, X"014", 0, v.pllCfg.bwSel);
      axiSlaveRegister(axilEp, X"014", 4, v.pllCfg.frqTbl);
      axiSlaveRegister(axilEp, X"014", 8, v.pllCfg.frqSel);
      axiSlaveRegister(axilEp, X"014", 16, v.pllCfg.rate);
      axiSlaveRegister(axilEp, X"014", 20, v.pllCfg.inc);
      axiSlaveRegister(axilEp, X"014", 21, v.pllCfg.dec);
      axiSlaveRegister(axilEp, X"014", 22, v.pllCfg.bypass);
      axiSlaveRegister(axilEp, X"014", 23, v.pllCfg.rstn);
      axiSlaveRegisterR(axilEp, X"014", 24, muxSlVectorArray(pllCount, 2*ia+0));
      axiSlaveRegisterR(axilEp, X"014", 27, pllStat(2*ia+0));
      axiSlaveRegisterR(axilEp, X"014", 28, muxSlVectorArray(pllCount, 2*ia+1));
      axiSlaveRegisterR(axilEp, X"014", 31, pllStat(2*ia+1));

      --axiSlaveRegister (axilEp, X"018", 0, v.partitionCfg.l0Select.reset);
      axiSlaveRegister (axilEp, X"018", 16, v.partitionCfg.l0Select.enabled);
      axiSlaveRegister (axilEp, X"018", 30, v.partitionCfg.master);
      axiSlaveRegister (axilEp, X"018", 31, v.axilRdEn(ip));

      axiSlaveRegister (axilEp, X"01C", 0, v.partitionCfg.l0Select.rateSel);
      axiSlaveRegister (axilEp, X"01C", 16, v.partitionCfg.l0Select.destSel);

      axiSlaveRegisterR(axilEp, X"020", 0, s.partition(ip).l0Select.enabled);
      axiSlaveRegisterR(axilEp, X"028", 0, s.partition(ip).l0Select.inhibited);
      axiSlaveRegisterR(axilEp, X"030", 0, s.partition(ip).l0Select.num);
      axiSlaveRegisterR(axilEp, X"038", 0, s.partition(ip).l0Select.numInh);
      axiSlaveRegisterR(axilEp, X"040", 0, s.partition(ip).l0Select.numAcc);
      axiSlaveRegisterR(axilEp, X"048", 0, s.partition(ip).l1Select.numAcc);

      --axiSlaveRegister (axilEp, X"050",  0, v.partitionCfg.l1Select.clear);
      --axiSlaveRegister (axilEp, X"050", 16, v.partitionCfg.l1Select.enable);

      --axiSlaveRegister (axilEp, X"054",  0, v.partitionCfg.l1Select.trigsrc);
      --axiSlaveRegister (axilEp, X"054",  4, v.partitionCfg.l1Select.trigword);
      --axiSlaveRegister (axilEp, X"054", 16, v.partitionCfg.l1Select.trigwr);

      axiSlaveRegister (axilEp, X"06C", 0, v.partitionCfg.pipeline.depth_clks);
      axiSlaveRegister (axilEp, X"06C", 16, v.partitionCfg.pipeline.depth_fids);

      --axiSlaveRegister (axilEp, X"070", 15, v.partitionCfg.message.insert);
      axiSlaveRegister (axilEp, X"070",  0, v.partitionCfg.message.header);

      axiSlaveRegisterR (axilEp, X"078", 0, r.linkStat.rxId);

      for j in r.partitionCfg.inhibit.setup'range loop
         axiSlaveRegister (axilEp, X"080" + toSlv(j*4, 12), 0, v.partitionCfg.inhibit.setup(j).interval);
         axiSlaveRegister (axilEp, X"080" + toSlv(j*4, 12), 12, v.partitionCfg.inhibit.setup(j).limit);
         axiSlaveRegister (axilEp, X"080" + toSlv(j*4, 12), 31, v.partitionCfg.inhibit.setup(j).enable);
      end loop;

      for j in 0 to 31 loop
         axiSlaveRegisterR (axilEp, X"090" + toSlv(j*4, 12), 0, r.partitionStat.inhibit.evcounts(j));
      end loop;

      for j in 0 to 3 loop
         axiSlaveRegisterR (axilEp, X"110" + toSlv(j*4, 12), 0, monClkRate(j)(28 downto 0));
         axiSlaveRegisterR (axilEp, X"110" + toSlv(j*4, 12), 29, monClkSlow(j));
         axiSlaveRegisterR (axilEp, X"110" + toSlv(j*4, 12), 30, monClkFast(j));
         axiSlaveRegisterR (axilEp, X"110" + toSlv(j*4, 12), 31, monClkLock(j));
      end loop;

      for j in 0 to 31 loop
         axiSlaveRegisterR (axilEp, X"120" + toSlv(j*4, 12), 0, r.partitionStat.inhibit.tmcounts(j));
      end loop;

      axiSlaveRegister (axilEp, X"1A0",  0, v.monStreamPeriod);
      axiSlaveRegister (axilEp, X"1A0", 31, v.monStreamEnable);
      axiSlaveRegisterR(axilEp, X"1A4",  0, monCount);
      axiSlaveRegisterR(axilEp, X"1A8",  0, monIndex);
      axiSlaveRegisterR(axilEp, X"1A8", 31, monBusy);
      axiSlaveRegisterR(axilEp, X"1AC",  0, monId);
      
      groupL0Reset   := (others => '0');
      groupL0Enable  := (others => '0');
      groupL0Disable := (others => '0');
      groupMsgInsert := (others => '0');

      axiSlaveRegister (axilEp, X"200", 0, groupL0Reset);
      axiSlaveRegister (axilEp, X"204", 0, groupL0Enable);
      axiSlaveRegister (axilEp, X"208", 0, groupL0Disable);
      axiSlaveRegister (axilEp, X"20C", 0, groupMsgInsert);

      v.stepMaster.tLast  := '1';
      ssiSetUserSof (EMAC_AXIS_CONFIG_C, v.stepMaster, '1');
      ssiSetUserEofe(EMAC_AXIS_CONFIG_C, v.stepMaster, '0');
      
      if obDebugSlave.tReady = '1' then
         v.stepMaster.tValid := '0';
      end if;
      
      for i in 0 to XPM_PARTITIONS_C-1 loop
         if stepDone(i) = '0' and r.step(i).groups /= 0 then
            v.step(i).enable := '1';
         end if;
         if stepDone(i) = '1' and r.step(i).enable = '1' and v.stepMaster.tValid = '0' then
            v.step(i).enable  := '0';
            for j in 0 to XPM_PARTITIONS_C-1 loop
               groupL0Disable(j) := r.step(i).groups(j);
            end loop;
            v.stepMaster.tValid := '1';
            v.stepMaster.tData(31 downto  0) := toSlv(i,16) & toSlv(1,16);
            v.stepMaster.tData(63 downto 32) := r.step(i).numL0Acc;
         end if;
      end loop;
      
      for i in 0 to XPM_PARTITIONS_C-1 loop
         if groupL0Reset(i) = '1' then
            v.config.partition(i).l0Select.reset := '1';
            v.load                               := '1';
         end if;
         if groupL0Enable(i) = '1' then
            v.config.partition(i).l0Select.enabled := '1';
            v.load                                 := '1';
         end if;
         if groupL0Disable(i) = '1' then
            v.config.partition(i).l0Select.enabled := '0';
            v.load                                 := '1';
         end if;
         if groupMsgInsert(i) = '1' then
            v.config.partition(i).message.insert := '1';
            v.load                               := '1';
         end if;
      end loop;

      for i in 0 to XPM_PARTITIONS_C-1 loop
         axiSlaveRegister (axilEp, toSlv(528+8*i+0,12), 0, v.step(i).groups);
         axiSlaveRegister (axilEp, toSlv(528+8*i+4,12), 0, v.step(i).numL0Acc);
      end loop;
      
      if r.link(4) = '0' and r.linkStat.rxIsXpm = '0' then
         v.linkCfg.groupMask := v.linkCfg.groupMask and not groupLinkClear;
      end if;
      for i in 0 to NUM_DS_LINKS_G-1 loop
         if status.dsLink(i).rxIsXpm = '0' then
            v.config.dsLink(i).groupMask := v.config.dsLink(i).groupMask and not groupLinkClear;
         end if;
      end loop;

--if r.partitionCfg.analysis.rst(1)='1' then
--  v.anaWrCount(ip) := (others=>'0');
--elsif r.partitionCfg.analysis.push(1)='1' then
--  v.anaWrCount(ip) := r.anaWrCount(ip)+1;
--end if;

--if r.config.tagstream='0' then
--  v.tagSlave.tReady := '0';
--elsif tagMaster.tValid='1' then
--  ip := conv_integer(tagMaster.tDest(3 downto 0));
--  v.config.partition(ip).analysis.tag  := tagMaster.tData(31 downto  0);
--  v.config.partition(ip).analysis.push := tagMaster.tData(35 downto 34);
--end if;
      v.tagSlave.tReady := '0';

-- Set the status
      axiSlaveDefault(axilEp, v.axilWriteSlave, v.axilReadSlave);

----------------------------------------------------------------------------------------------
-- Reset
----------------------------------------------------------------------------------------------
      if (axilRst = '1') then
         v := REG_INIT_C;
      end if;

      r_in <= v;
   end process;

   seq : process (axilClk) is
   begin
      if rising_edge(axilClk) then
         r <= r_in after TPD_G;
      end if;
   end process;

   U_MONSTREAM : entity l2si.XpmMonitorStream
     port map (
      axilClk         => axilClk,
      axilRst         => axilRst,
      enable          => r.monStreamEnable,
      period          => r.monStreamPeriod,
      pllCount        => pllCount,
      pllStat         => pllStat,
      monClkRate      => monClkRate,
      status          => status,
      staClk          => staClk,
      -- status
      busy            => monBusy,
      count           => monCount,
      id              => monId,
      index           => monIndex,
      -- Application Debug Interface (sysclk domain)
      obMonitorMaster => obMonitorMaster,
      obMonitorSlave  => obMonitorSlave );
   
   rcomb : process ( staRst, q, step, status ) is
      constant STATUS_INTERVAL_C : slv(19 downto 0) := toSlv(910000-1, 20);
      variable v : QRegType;
   begin
      v := q;

      if q.cnt = STATUS_INTERVAL_C then
         v.cnt       := (others=>'0');
         v.staUpdate := '1';
      else
         v.cnt       := q.cnt + 1;
         v.staUpdate := '0';
      end if;

      for i in 0 to XPM_PARTITIONS_C-1 loop
         v.stepDone(i) := '0';
         if status.partition(i).l0Select.numAcc = step(i).numL0Acc then
           v.stepDone(i) := '1';
         end if;
      end loop;
      
      if staRst = '1' then
         v := QREG_INIT_C;
      end if;
      
      q_in <= v;
   end process rcomb;
   
   rseq : process (staClk) is
   begin
      if rising_edge(staClk) then
         q <= q_in;
      end if;
   end process rseq;
   
   STEP_SYNC : for i in 0 to XPM_PARTITIONS_C-1 generate
      U_SyncEnable : entity surf.Synchronizer
        port map ( clk     => staClk,
                   dataIn  => r.step(i).enable,
                   dataOut => step(i).enable );
      U_SyncNumL0 : entity surf.SynchronizerVector
         generic map ( WIDTH_G => 32 )
         port map ( clk     => staClk,
                    dataIn  => r.step(i).numL0Acc,
                    dataOut => step(i).numL0Acc );
      U_SyncGroups : entity surf.SynchronizerVector
         generic map ( WIDTH_G => XPM_PARTITIONS_C )
         port map ( clk     => staClk,
                    dataIn  => r.step(i).groups,
                    dataOut => step(i).groups );
      U_SyncDone : entity surf.Synchronizer
         port map ( clk     => axilClk,
                    dataIn  => q.stepDone(i),
                    dataOut => stepDone(i) );
   end generate;
   
end rtl;
