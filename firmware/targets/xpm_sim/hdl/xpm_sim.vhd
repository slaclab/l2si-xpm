-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : xpm_sim.vhd
-- Author     : Matt Weaver <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-10
-- Last update: 2024-01-11
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: XpmApp's Top Level
-- 
-- Note: Common-to-XpmApp interface defined here (see URL below)
--       https://confluence.slac.stanford.edu/x/rLyMCw
-------------------------------------------------------------------------------
-- This file is part of 'LCLS2 DAQ Software'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'LCLS2 DAQ Software', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;


library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;
use surf.AxiStreamPkg.all;
use surf.SsiPkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;
use lcls_timing_core.TPGPkg.all;
use lcls_timing_core.TPGMiniEdefPkg.all;

library l2si_core;
use l2si_core.L2SiPkg.all;
use l2si_core.XpmPkg.all;
use l2si_core.XpmMiniPkg.all;

library l2si;
use l2si.AxiLiteSimPkg.all;

library unisim;
use unisim.vcomponents.all;

entity xpm_sim is
end xpm_sim;

architecture top_level_app of xpm_sim is

   constant DMA_AXIS_CONFIG_C : AxiStreamConfigType := ssiAxiStreamConfig(8, TKEEP_COMP_C, TUSER_FIRST_LAST_C, 8, 2);
   
   constant NDsLinks : integer := 14;
  
   signal tpgConfig : TPGConfigType := TPG_CONFIG_INIT_C;
   signal xpmConfig : XpmMiniConfigType := XPM_MINI_CONFIG_INIT_C;
   signal appConfig : XpmConfigType := XPM_CONFIG_INIT_C;
   
   signal regClk         : sl;
   signal regRst         : sl;
   signal axiRst         : sl;

   signal dsClk, dsRst : slv(NDsLinks-1 downto 0);
   signal dsRx       : TimingRxArray (NDsLinks-1 downto 0) := (others=>TIMING_RX_INIT_C);
   signal dsTx       : TimingPhyArray(NDsLinks-1 downto 0) := (others=>TIMING_PHY_INIT_C);

   signal fbPhy      : TimingPhyType := TIMING_PHY_INIT_C;
   signal fbPause    : slv(XPM_PARTITIONS_C-1 downto 0) := (others=>'0');
   
   signal scClk, scRst : sl;
   signal usClk, usRst : sl;
   signal usRx  : TimingRxType := TIMING_RX_INIT_C;
   signal usRxStatus : TimingPhyStatusType := TIMING_PHY_STATUS_INIT_C;
   
   signal xpmStream  : XpmStreamType := XPM_STREAM_INIT_C;
   signal timingStream  : XpmStreamType := XPM_STREAM_INIT_C;
   signal xData      : TimingRxType := TIMING_RX_INIT_C;
   signal xDataStatus: TimingPhyStatusType := TIMING_PHY_STATUS_INIT_C;

   signal tpgStream   : TimingSerialType;
   signal tpgAdvance  : sl;
   signal tpgFiducial : sl;
   
   type RegType is record
     advance : sl;
   end record;

   constant REG_INIT_C : RegType := ( advance => '0' );

   signal r    : RegType := REG_INIT_C;
   signal rin  : RegType;

   signal pllCount : SlVectorArray(3 downto 0,2 downto 0) := (others=>(others=>'0'));
   signal pllStat  : slv(3 downto 0) := "1011";
   signal monClkRate : Slv32Array(3 downto 0) := ( toSlv(3,32), toSlv(2,32), toSlv(1,32), toSlv(0,32));
   signal status     : XpmStatusType := XPM_STATUS_INIT_C;
   signal obMonitorMaster : AxiStreamMasterType;
   signal obMonitorSlave  : AxiStreamSlaveType := AXI_STREAM_SLAVE_FORCE_C;

   signal eventTrigMsgMasters   : AxiStreamMasterArray(0 downto 0);
   signal eventTrigMsgSlaves    : AxiStreamSlaveArray(0 downto 0);

   signal eventTrigMsgMastersG  : AxiStreamMasterArray(0 downto 0);
   signal eventTrigMsgSlavesG   : AxiStreamSlaveArray(0 downto 0);

   signal eventTimingMessagesValid : slv(0 downto 0);
   signal eventTimingMessages      : TimingMessageArray(0 downto 0);
   signal eventTimingMessagesRd    : slv(0 downto 0);
   
   signal eventTimingMsgMasters : AxiStreamMasterArray(0 downto 0);
   signal eventTimingMsgSlaves  : AxiStreamSlaveArray(0 downto 0);
   
   signal triggerData : TriggerEventDataArray(0 downto 0);
   signal trgMaster   : AxiStreamMasterType := AxiStreamMasterInit(DMA_AXIS_CONFIG_C);
   signal trgSlave    : AxiStreamSlaveType;
   signal detMaster   : AxiStreamMasterType;
   signal detSlave    : AxiStreamSlaveType;
   
   signal clearReadout : slv(0 downto 0);

   signal appTimingBus : TimingBusType;
   signal appTimingMode : sl;

   signal eventMaster : AxiStreamMasterType;
   signal eventSlave  : AxiStreamSlaveType;

   signal temWriteMaster : AxiLiteWriteMasterType;
   signal temWriteSlave  : AxiLiteWriteSlaveType;

   signal seqReadMaster  : AxiLiteReadMasterType;
   signal seqReadSlave   : AxiLiteReadSlaveType;
   signal seqWriteMaster : AxiLiteWriteMasterType;
   signal seqWriteSlave  : AxiLiteWriteSlaveType;
   signal seqWriteDone, seqWriteNotDone : sl;
   signal seqCountRst    : sl;

   signal paddr : slv(31 downto 0) := x"DEADBEEF";
   signal scClkA  : slv(6 downto 0);
   signal scRstA  : slv(6 downto 0);

   signal nscClk     : sl;
   signal nscRst     : sl;
   signal dsTxDataS  : slv(17 downto 0);
   signal dsTxData   : slv(15 downto 0);
   signal dsTxDataK  : slv( 1 downto 0);

begin

  tpgConfig.FixedRateDivisors <= (toSlv(0,20),
                                  toSlv(0,20),
                                  toSlv(64,20),
                                  toSlv(64,20),
                                  toSlv(32,20),
                                  toSlv(16,20),
                                  toSlv(8,20),
                                  toSlv(4,20),
                                  toSlv(2,20),
                                  toSlv(1,20));
  
   xpmConfig.partition.l0Select.enabled <= '1';
   xpmConfig.partition.l0Select.rateSel <= x"0002";
   xpmConfig.partition.l0Select.destSel <= x"8000";
   xpmConfig.partition.pipeline.depth_fids <= toSlv(90,8);
   xpmConfig.partition.pipeline.depth_clks <= toSlv(90*200,16);

   xpmConfig.dsLink(0).enable    <= '1';

   process is
   begin
     for i in 0 to 7 loop
       appConfig.partition(i).master              <= '1';
       appConfig.partition(i).l0Select.enabled    <= '0';
       appConfig.partition(i).l0Select.rateSel    <= toSlv(i,16);
       appConfig.partition(i).l0Select.destSel    <= x"8000";
--       appConfig.partition(i).pipeline.depth_fids <= toSlv(10-i,8);
--       appConfig.partition(i).pipeline.depth_clks <= toSlv((10-i)*200,16);
       appConfig.partition(i).pipeline.depth_fids <= toSlv(10,8);
       appConfig.partition(i).pipeline.depth_clks <= toSlv(10*200,16);
       appConfig.partition(i).l0Select.rawPeriod  <= toSlv(720-i*100,20);
--       appConfig.partition(i).l0Select.rawPeriod  <= toSlv(ite(i=7,20,10000),20);
       appConfig.partition(i).l0Select.groups     <= x"FF";
--       appConfig.partition(i).l0Select.groups     <= x"00";
     end loop;
   
     wait for 25 us;
     for i in 0 to 7 loop
       appConfig.partition(i).message.header <= toSlv(0,9);
     end loop;
     wait until regClk='0';
     for i in 0 to 7 loop
       appConfig.partition(i).message.insert <= '1';
--       appConfig.partition(i).message.insert <= '0';
     end loop;
     wait until regClk='1';
     wait until regClk='0';
     for i in 0 to 7 loop
       appConfig.partition(i).message.insert <= '0';
     end loop;
     wait for 20 us;
     for i in 0 to 7 loop
       appConfig.partition(i).l0Select.enabled <= '1';
     end loop;
     wait;
   end process;
   
   process is
   begin
     regRst <= '1';
     axiRst <= '1';
     xpmConfig.partition.l0Select.reset <= '1';
     wait for 100 ns;
     regRst <= '0';
     wait for 100 ns;
     axiRst <= '0';
     xpmConfig.partition.l0Select.reset <= '0';
     tpgConfig.pulseIdWrEn <= '0';
     wait for 100 ns;

     --  Clear Readout (and put the FIFOs into a known state)
     xpmConfig.partition.message.header <= toSlv(0,9);
     wait until regClk='0';
     xpmConfig.partition.message.insert <= '1';
     wait until regClk='1';
     wait until regClk='0';
     xpmConfig.partition.message.insert <= '0';

     --  Send transitions (avoiding ClearReadout)
     wait for 180 us;
     for i in 0 to 100 loop
       --  Insert transition, even if inhibited
       xpmConfig.partition.message.header <= XPM_PART_MSG_DEFAULT_C &
                                             "00" & toSlv(i,5); 
       wait until regClk='0';
       xpmConfig.partition.message.insert <= '1';
       wait until regClk='1';
       wait until regClk='0';
       xpmConfig.partition.message.insert <= '0';
       wait for 5 us;

       --  Insert transition, only if not inhibited (can be dropped)
       xpmConfig.partition.message.header <= XPM_PART_MSG_DROP_FULL_C &
                                             "10" & toSlv(i,5);
       wait until regClk='0';
       xpmConfig.partition.message.insert <= '1';
       wait until regClk='1';
       wait until regClk='0';
       xpmConfig.partition.message.insert <= '0';
       wait for 5 us;

       --  Insert transition, waiting for not inhibited
       xpmConfig.partition.message.header <= XPM_PART_MSG_WAIT_FULL_C &
                                             "11" & toSlv(i,5);
       wait until regClk='0';
       xpmConfig.partition.message.insert <= '1';
       wait until regClk='1';
       wait until regClk='0';
       xpmConfig.partition.message.insert <= '0';
       wait for 5 us;

       --  Insert transition, waiting for not inhibited and no L0
       xpmConfig.partition.message.header <= XPM_PART_MSG_LOW_PRIO_C &
                                             "01" & toSlv(i,5);
       wait until regClk='0';
       xpmConfig.partition.message.insert <= '1';
       wait until regClk='1';
       wait until regClk='0';
       xpmConfig.partition.message.insert <= '0';
       wait for 5 us;
     end loop;
     wait;
   end process;

   process is
   begin
     seqCountRst <= '0';
     wait for 10 us;
     seqCountRst <= '1';
     wait for 10 ns;
   end process;
   
   process is
   begin
     wait for 220 us;
     for i in 0 to 100 loop
       fbPause(0) <= '1';
       wait for 5 us;
       fbPause(0) <= '0';
       wait for 7 us;
     end loop;
     wait;
   end process;
   
   process is
   begin
     regClk <= '1';
     wait for 4.0 ns;
     regClk <= '0';
     wait for 4.0 ns;
   end process;
     
   process is
   begin
     scClk <= '1';
     wait for 2.69 ns;
     scClk <= '0';
     wait for 2.69 ns;
   end process;
   scRst <= regRst;

   scClkA <= (others=>scClk);
   scRstA <= (others=>scRst);
   
   U_TemConfig : entity l2si.AxiLiteWriteMasterSim
     generic map ( CMDS => (( addr  => x"0000900C",
                              value => toSlv(250*14,32)),
                            ( addr  => x"00009000",
                              value => x"00000001")) )
     port map ( clk    => regClk,
                rst    => regRst,
                master => temWriteMaster,
                slave  => temWriteSlave,
                done   => open );

   usClk <= not scClk;
   usRst <= scRst;

   dsClk <= (others=>usClk);
   dsRst <= (others=>usRst);

   U_UsSim : entity lcls_timing_core.TPGMini
     generic map ( NARRAYSBSA => 0,
                   STREAM_INTF => true )
     port map ( statusO   => open,
                configI   => tpgConfig,
                --
                txClk     => usClk,
                txRst     => usRst,
                txRdy     => '1',
                streams(0)=> tpgStream,
                advance(0)=> tpgAdvance,
                fiducial  => tpgFiducial );

   xpmStream.fiducial   <= tpgFiducial;
   xpmStream.advance(0) <= tpgAdvance;
   xpmStream.streams(0) <= tpgStream;

   U_Application : entity l2si_core.XpmMini
     generic map (
       NUM_DS_LINKS_G  => NDsLinks )
     port map (
       regclk          => regClk,
       regrst          => regRst,
       update          => '1',
       status          => open,
       config          => xpmConfig,
       -- DS Ports
       dsRxClk         => dsClk,
       dsRxRst         => dsRst,
       dsRx            => dsRx,
       dsTx            => dsTx,
       -- Timing Interface (timingClk domain) 
       timingClk       => usClk,
       timingRst       => usRst,
       timingStream    => xpmStream );

   U_TimingFb : entity l2si_core.XpmTimingFb
     port map (
       clk         => usClk,
       rst         => usRst,
       pause       => fbPause,
       phy         => fbPhy );

   dsRx(0).data  <= fbPhy.data;
   dsRx(0).dataK <= fbPhy.dataK;
   
--   U_WriteX : entity work.XpmFile
--     generic map ( filename => "xpmmini.dat" )
--     port map ( clk  => dsClk(0),
--                data => dsTx(0).data );

   comb : process ( r, usRst, tpgFiducial, tpgStream ) is
     variable v : RegType;
   begin
     v := r;

     v.advance := tpgStream.ready and not tpgFiducial;
     
     if usRst = '1' then
       v := REG_INIT_C;
     end if;

     rin <= v;

     tpgAdvance <= v.advance;
   end process;

   seq : process ( usClk )
   begin
     if rising_edge(usClk) then
       r <= rin;
     end if;
   end process;

   
   U_TimingCore : entity lcls_timing_core.TimingCore
      generic map (
         TPD_G             => 1 ns,
         DEFAULT_CLK_SEL_G => '1',
         TPGEN_G           => false,
         AXIL_RINGB_G      => false,
         ASYNC_G           => true )
      port map (
         -- GT Interface
         gtTxUsrClk       => usClk,
         gtTxUsrRst       => usRst,
         gtRxRecClk       => dsClk(0),
         gtRxData         => dsTx(0).data,
         gtRxDataK        => dsTx(0).dataK,
         gtRxDispErr      => "00",
         gtRxDecErr       => "00",
         gtRxControl      => open,
         gtRxStatus       => TIMING_PHY_STATUS_FORCE_C,
         tpgMiniTimingPhy => open,
         timingClkSel     => open,
         -- Decoded timing message interface
         appTimingClk     => dsClk(0),
         appTimingRst     => dsRst(0),
         appTimingMode    => appTimingMode,
         appTimingBus     => appTimingBus,
         -- AXI Lite interface
         axilClk          => regClk,
         axilRst          => regRst,
         axilReadMaster   => AXI_LITE_READ_MASTER_INIT_C,
         axilReadSlave    => open,
         axilWriteMaster  => AXI_LITE_WRITE_MASTER_INIT_C,
         axilWriteSlave   => open );

   U_TriggerEventManager_1 : entity l2si_core.TriggerEventManager
      generic map (
         TPD_G                          => 1 ns,
         EN_LCLS_I_TIMING_G             => false,
         EN_LCLS_II_TIMING_G            => true,
         NUM_DETECTORS_G                => 1,
         L1_CLK_IS_TIMING_TX_CLK_G      => false,
         TRIGGER_CLK_IS_TIMING_RX_CLK_G => false,
         EVENT_CLK_IS_TIMING_RX_CLK_G   => false)
      port map (
         timingRxClk              => dsClk(0),
         timingRxRst              => dsRst(0),
         timingBus                => appTimingBus,                   -- [in]
         timingMode               => appTimingMode,                  -- [in]
         timingTxClk              => usClk,
         timingTxRst              => usRst,
         timingTxPhy              => open,
         triggerClk               => regClk,
         triggerRst               => regRst,
         triggerData              => triggerData,                    -- [out]
         clearReadout             => clearReadout,                   -- [out]
         l1Clk                    => dsClk(0),
         l1Rst                    => dsRst(0),
         l1Acks                   => open,
         eventClk                 => regClk,
         eventRst                 => regRst,
         eventTimingMessagesValid => eventTimingMessagesValid,
         eventTimingMessages      => eventTimingMessages,
         eventTimingMessagesRd    => eventTimingMessagesRd,
         eventAxisMasters         => eventTrigMsgMasters,            -- [out]
         eventAxisSlaves          => eventTrigMsgSlaves,             -- [in]
         eventAxisCtrl            => (others => AXI_STREAM_CTRL_UNUSED_C),
         axilClk                  => regClk,
         axilRst                  => regRst,
         axilReadMaster           => AXI_LITE_READ_MASTER_INIT_C,
         axilReadSlave            => open,
         axilWriteMaster          => temWriteMaster,
         axilWriteSlave           => temWriteSlave );

   U_Resize : entity surf.AxiStreamGearbox
     generic map (
       -- AXI Stream Port Configurations
       SLAVE_AXI_CONFIG_G  => EVENT_AXIS_CONFIG_C,
       MASTER_AXI_CONFIG_G => DMA_AXIS_CONFIG_C)
     port map (
       axisClk     => regClk,
       axisRst     => regRst,
       -- Slave Port
       sAxisMaster => eventTrigMsgMasters(0),
       sAxisSlave  => eventTrigMsgSlaves (0),
       -- Master Port
       mAxisMaster => eventTrigMsgMastersG(0),
       mAxisSlave  => eventTrigMsgSlavesG (0));

   U_EventTimingMessage : entity l2si_core.EventTimingMessage
     generic map (
       TPD_G               => 1 ns,
       NUM_DETECTORS_G     => 1,
       EVENT_AXIS_CONFIG_G => DMA_AXIS_CONFIG_C )
     port map (
       -- Clock and Reset
       eventClk                 => regClk,
       eventRst                 => regRst,
       -- Input Streams
       eventTimingMessagesValid => eventTimingMessagesValid,
       eventTimingMessages      => eventTimingMessages,
       eventTimingMessagesRd    => eventTimingMessagesRd,
       -- Output Streams
       eventTimingMsgMasters    => eventTimingMsgMasters,
       eventTimingMsgSlaves     => eventTimingMsgSlaves );
   
   U_EventBuilder : entity surf.AxiStreamBatcherEventBuilder
      generic map (
         TPD_G          => 1 ns,
         NUM_SLAVES_G   => 3,
         MODE_G         => "ROUTED",
         TDEST_ROUTES_G => (
            0           => "0000000-",   -- Trig on 0x0, Event on 0x1
            1           => "00000010",   -- Map PGP[VC1] to TDEST 0x2
            2           => "00000011"),  -- Map Timing   to TDEST 0x3
         TRANS_TDEST_G  => X"01",
         AXIS_CONFIG_G  => DMA_AXIS_CONFIG_C )
      port map (
         -- Clock and Reset
         axisClk         => regClk,
         axisRst         => regRst,
         -- AXI-Lite Interface (axisClk domain)
         axilReadMaster  => AXI_LITE_READ_MASTER_INIT_C,
         axilReadSlave   => open,
         axilWriteMaster => AXI_LITE_WRITE_MASTER_INIT_C,
         axilWriteSlave  => open,
         -- AXIS Interfaces
         sAxisMasters(0) => eventTrigMsgMastersG(0),
         sAxisMasters(1) => detMaster,
         sAxisMasters(2) => eventTimingMsgMasters(0),
         sAxisSlaves(0)  => eventTrigMsgSlavesG(0),
         sAxisSlaves(1)  => detSlave,
         sAxisSlaves(2)  => eventTimingMsgSlaves(0),
         mAxisMaster     => eventMaster,
         mAxisSlave      => eventSlave);

   U_File : entity l2si.AxiStreamFile
     generic map ( filename => "xpm_sim.xtc" )
     port map ( axisClk     => regClk,
                axisMaster  => eventMaster,
                axisSlave   => eventSlave );

   eventSlave.tReady <= '1';
   
   trgMaster.tValid <= triggerData(0).valid and triggerData(0).l0Accept;
   trgMaster.tLast  <= '1';
   trgMaster.tData  <= toSlv(0,trgMaster.tData'length-24) & triggerData(0).count;

   U_TrgData : entity surf.AxiStreamFifoV2
      generic map (
         TPD_G               => 1 ns,
         INT_PIPE_STAGES_G   => 1,
         PIPE_STAGES_G       => 1,
         SLAVE_READY_EN_G    => false,
         MEMORY_TYPE_G       => "block",
         GEN_SYNC_FIFO_G     => true,
         FIFO_ADDR_WIDTH_G   => 5,
         FIFO_FIXED_THRESH_G => true,
         FIFO_PAUSE_THRESH_G => 16,
         SLAVE_AXI_CONFIG_G  => DMA_AXIS_CONFIG_C,
         MASTER_AXI_CONFIG_G => DMA_AXIS_CONFIG_C)
      port map (
         sAxisClk        => regClk,        -- [in]
         sAxisRst        => clearReadout(0),
         sAxisMaster     => trgMaster,
         sAxisSlave      => trgSlave,
         sAxisCtrl       => open,
         fifoWrCnt       => open,
         mAxisClk        => regClk,
         mAxisRst        => regRst,
         mAxisMaster     => detMaster,
         mAxisSlave      => detSlave);

   --  0x0000 SeqState
   --  0x4000 SeqJump
   --  0x8000 SeqMem
   U_SeqConfig : entity l2si.AxiLiteWriteMasterSim
     generic map ( CMDS => (( addr  => x"00010000", value => x"00000004"),-- branch to line 4
                            ( addr  => x"00010004", value => x"80000abc"),-- request
                            ( addr  => x"00010008", value => x"40000002"),-- sync
                            ( addr  => x"0001000c", value => x"A0001000"),-- return
                            ( addr  => x"00010010", value => x"40000009"),-- sync
                            ( addr  => x"00010014", value => x"A0000001"),-- call
                            ( addr  => x"00010018", value => x"00000005"),-- branch
                            ( addr  => x"00018000", value => x"00000004"),-- branch to line 4
                            ( addr  => x"00018004", value => x"80000def"),-- request
                            ( addr  => x"00018008", value => x"40000002"),-- sync
                            ( addr  => x"0001800c", value => x"A0001000"),-- return
                            ( addr  => x"00018010", value => x"40000009"),-- sync
                            ( addr  => x"00018014", value => x"A0000001"),-- call
                            ( addr  => x"00018018", value => x"00000005"),-- branch
                            ( addr  => x"0000403c", value => x"00000000"),
                            ( addr  => x"0000413c", value => x"00000000"),
                            ( addr  => x"00000004", value => x"00000011"),
                            ( addr  => x"00000008", value => x"00000011")) )
     port map ( clk    => regClk,
                rst    => axiRst,
                master => seqWriteMaster,
                slave  => seqWriteSlave,
                done   => seqWriteDone );

   seqWriteNotDone <= not seqWriteDone;
     
   U_SeqStatus : entity l2si.AxiLiteReadMasterSim
     generic map ( CMDS => (x"00008000",
                            x"00008004",
                            x"0000403c") )
     port map ( clk    => regClk,
                rst    => seqWriteNotDone,
                master => seqReadMaster,
                slave  => seqReadSlave,
                done   => open );
   
   U_App : entity l2si_core.XpmApp
     generic map (
       NUM_BP_LINKS_G  => 1,
       AXIL_BASEADDR_G => (others => '0'))
     port map (
       -----------------------
       -- XpmApp Ports --
       -----------------------
       regclk          => regClk,
       regrst          => regRst,
       update          => (others=>'0'),
       config          => appConfig,
       axilReadMaster  => seqReadMaster,
       axilReadSlave   => seqReadSlave,
       axilWriteMaster => seqWriteMaster,
       axilWriteSlave  => seqWriteSlave,
       obAppSlave      => AXI_STREAM_SLAVE_INIT_C,
       -- AMC's DS Ports
       dsLinkStatus    => (others=>XPM_LINK_STATUS_INIT_C),
       dsRxData        => (others=>x"0000"),
       dsRxDataK       => (others=>"00"),
       dsTxData    (0) => dsTxData,
       dsTxDataK   (0) => dsTxDataK,
       dsRxErr         => (others=>'0'),
       dsRxClk         => scClkA,
       dsRxRst         => scRstA,
       --  BP DS Ports
       bpStatus        => (others=>XPM_BP_LINK_STATUS_INIT_C),
       bpRxLinkPause   => (others=>x"0000"),
       -- Timing Interface (timingClk domain)
       timingClk       => scClk,
       timingRst       => scRst,
--      timingIn          : in  TimingRxType;
       timingStream    => xpmStream,
       timingFbClk     => scClk,
       timingFbRst     => scRst,
       timingFbId      => x"DEADBEEF",
       seqCountRst     => seqCountRst );

   process is
   begin
     nscRst <= '1';
     wait for 100 ns;
     nscRst <= '0';
     wait for 5000 ns;
   end process;
     
   nscClk <= not scClk;
   
   U_Fifo : entity surf.SynchronizerFifo
     generic map ( DATA_WIDTH_G => 18,
                   ADDR_WIDTH_G => 4 )
     port map ( rst               => nscRst,
                wr_clk            => scClk,
                din(17 downto 16) => dsTxDataK,
                din(15 downto  0) => dsTxData,
                rd_clk            => nscClk,
                dout              => dsTxDataS );
   
end top_level_app;
