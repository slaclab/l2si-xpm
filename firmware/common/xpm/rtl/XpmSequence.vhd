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
use surf.AxiStreamPkg.all;
use surf.SsiPkg.all;
use surf.EthMacPkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;
use lcls_timing_core.TPGPkg.all;

library l2si_core;
--use l2si_core.XpmSeqPkg.all;
use l2si_core.XpmPkg.all;

library l2si;
use l2si.XpmAppPkg.all;

entity XpmSequence is
   generic (
      TPD_G           : time             := 1 ns;
      NUM_SEQ_G       : natural          := 1;
      NUM_DDC_G       : integer          := 0;
      AXIL_BASEADDR_G : slv(31 downto 0) := (others => '0'));
   port (
      -- AXI-Lite Interface (on axiClk domain)
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType;
      obAppMaster     : out AxiStreamMasterType;
      obAppSlave      : in  AxiStreamSlaveType;
      seqRestart      : in  slv(NUM_SEQ_G-1 downto 0);
      seqDisable      : in  slv(NUM_SEQ_G-1 downto 0);
      -- Configuration/Status (on clk domain)
      timingClk       : in  sl;
      timingRst       : in  sl;
      timingAdvance   : in  sl;
      timingDataIn    : in  slv(15 downto 0);
      timingDataOut   : out slv(15 downto 0);
      seqCountRst     : in  sl := '0';
      seqCount        : out Slv128Array(NUM_SEQ_G+NUM_DDC_G-1 downto 0);
      seqInvalid      : out slv(NUM_SEQ_G-1 downto 0));
end XpmSequence;

architecture mapping of XpmSequence is

   constant SEQ_INDEX_C       : natural := 0;
   constant DDC_INDEX_C       : natural := 1;
   constant NUM_AXI_MASTERS_C : natural := 2;
   
   constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(NUM_AXI_MASTERS_C-1 downto 0) :=
     genAxiLiteConfig(NUM_AXI_MASTERS_C, AXIL_BASEADDR_G, 18, 17);
   signal axilWriteMasters    : AxiLiteWriteMasterArray(NUM_AXI_MASTERS_C-1 downto 0);
   signal axilWriteSlaves     : AxiLiteWriteSlaveArray (NUM_AXI_MASTERS_C-1 downto 0);
   signal axilReadMasters     : AxiLiteReadMasterArray (NUM_AXI_MASTERS_C-1 downto 0);
   signal axilReadSlaves      : AxiLiteReadSlaveArray  (NUM_AXI_MASTERS_C-1 downto 0);

   --  Handle NUM_DDC_G = 0
   constant NUM_DDC_C : natural := ite(NUM_DDC_G>0,NUM_DDC_G,1);
   constant DDC_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(NUM_DDC_C-1 downto 0) :=
     genAxiLiteConfig(NUM_DDC_C, AXI_CROSSBAR_MASTERS_CONFIG_C(DDC_INDEX_C).baseAddr, 17, 12);
   signal ddcAxilWriteMasters : AxiLiteWriteMasterArray(NUM_DDC_C-1 downto 0);
   signal ddcAxilWriteSlaves  : AxiLiteWriteSlaveArray (NUM_DDC_C-1 downto 0);
   signal ddcAxilReadMasters  : AxiLiteReadMasterArray (NUM_DDC_C-1 downto 0);
   signal ddcAxilReadSlaves   : AxiLiteReadSlaveArray  (NUM_DDC_C-1 downto 0);

   signal status       : XpmSeqStatusArray(NUM_SEQ_G-1 downto 0);
   signal gconfig      : XpmSeqGConfigType;
   signal config       : XpmSeqConfigArray(NUM_SEQ_G-1 downto 0);
   signal ddcData      : Slv4Array   (NUM_DDC_C-1 downto 0);
   signal seqData      : Slv4Array   (NUM_SEQ_G-1 downto 0);
   signal seqReset     : slv         (NUM_SEQ_G-1 downto 0);
   signal seqJump      : slv         (NUM_SEQ_G-1 downto 0);
   signal seqJumpAddr  : SeqAddrArray(NUM_SEQ_G-1 downto 0);
   signal frameSlv     : slv(TIMING_MESSAGE_BITS_C-1 downto 0);
   signal frame        : TimingMessageType;
   signal tframeSlv    : slv(TIMING_MESSAGE_BITS_C-1 downto 0);
   signal tframe       : TimingMessageType;
   signal sinkSlv      : Slv13Array  (NUM_SEQ_G-1 downto 0);
     
   constant S0 : integer := 12;
   constant SN : integer := S0+46;
   constant SEQBITS : integer := 4;

   type RegType is record
      advance : sl;
      frame   : slv(207 downto 0);      -- Really 64b
      framel  : slv(207 downto 0);
      strobe  : slv(SN downto 0);
      data    : slv(15 downto 0);
      master  : AxiStreamMasterType;
      ack     : slv (NUM_SEQ_G-1 downto 0);
      cnt     : slv (8 downto 0);
      cntl    : slv (8 downto 0);
      seqInvalid : slv (NUM_SEQ_G-1 downto 0);
   end record RegType;

   constant REG_INIT_C : RegType := (
      advance => '0',
      frame   => (others => '0'),
      framel  => (others => '0'),
      strobe  => (others => '0'),
      data    => (others => '0'),
      master  => axiStreamMasterInit(EMAC_AXIS_CONFIG_C),
      ack     => (others => '0'),
      cnt     => (others => '0'),
      cntl    => (others => '0'),
      seqInvalid => (others => '1'));

   signal r    : RegType := REG_INIT_C;
   signal r_in : RegType;

   signal seqNotifyValid : slv (NUM_SEQ_G-1 downto 0);
   signal seqNotify      : SeqAddrArray(NUM_SEQ_G-1 downto 0);

   signal axisSlave : AxiStreamSlaveType;

begin

   timingDataOut <= r_in.data;

   GEN_SEQCOUNT : for i in 0 to NUM_SEQ_G-1 generate
     seqCount(i+NUM_DDC_G) <= config(i).seqEnable &
                              status(i).countRequest(126 downto 0);
   end generate;

   seqInvalid         <= r.seqInvalid;
   
   U_AXIL_XBAR : entity surf.AxiLiteCrossbar
     generic map (
       MASTERS_CONFIG_G   => AXI_CROSSBAR_MASTERS_CONFIG_C,
       NUM_SLAVE_SLOTS_G  => 1,
       NUM_MASTER_SLOTS_G => NUM_AXI_MASTERS_C )
     port map (
      axiClk    => axilClk,
      axiClkRst => axilRst,
      -- Slave Slots (Connect to AxiLite Masters)
      sAxiWriteMasters(0) => axilWriteMaster,
      sAxiWriteSlaves (0) => axilWriteSlave,
      sAxiReadMasters (0) => axilReadMaster,
      sAxiReadSlaves  (0) => axilReadSlave,
      -- Master Slots (Connect to AXI Slaves)
      mAxiWriteMasters => axilWriteMasters,
      mAxiWriteSlaves  => axilWriteSlaves,
      mAxiReadMasters  => axilReadMasters,
      mAxiReadSlaves   => axilReadSlaves );
       
   U_FIFO : entity surf.AxiStreamFifoV2
      generic map (
         TPD_G               => TPD_G,
         SLAVE_AXI_CONFIG_G  => EMAC_AXIS_CONFIG_C,
         MASTER_AXI_CONFIG_G => EMAC_AXIS_CONFIG_C,
         FIFO_ADDR_WIDTH_G   => 4)
      port map (
         sAxisClk    => timingClk,
         sAxisRst    => timingRst,
         sAxisMaster => r.master,
         sAxisSlave  => axisSlave,
         mAxisClk    => axilClk,
         mAxisRst    => axilRst,
         mAxisMaster => obAppMaster,
         mAxisSlave  => obAppSlave);

   U_XBar : entity l2si.XpmSeqXbar
      generic map (
         TPD_G           => TPD_G,
         NUM_SEQ_G       => NUM_SEQ_G,
         AXIL_BASEADDR_G => AXI_CROSSBAR_MASTERS_CONFIG_C(SEQ_INDEX_C).baseAddr)
      port map (
         axiClk         => axilClk,
         axiRst         => axilRst,
         axiReadMaster  => axilReadMasters (SEQ_INDEX_C),
         axiReadSlave   => axilReadSlaves  (SEQ_INDEX_C),
         axiWriteMaster => axilWriteMasters(SEQ_INDEX_C),
         axiWriteSlave  => axilWriteSlaves (SEQ_INDEX_C),
         seqRestart     => seqRestart,
         seqDisable     => seqDisable,
         clk            => timingClk,
         rst            => timingRst,
         status         => status,
         gconfig        => gconfig,
         config         => config);

   frameSlv <= toSlv(0, TIMING_MESSAGE_BITS_C-r.framel'length) &
               r.framel;
   frame <= toTimingMessageType(frameSlv);

   tframeSlv <= toSlv(0, TIMING_MESSAGE_BITS_C-r.frame'length) &
                r.frame;
   tframe <= toTimingMessageType(tframeSlv);

   GEN_DDC : if NUM_DDC_G > 0 generate
     U_DDC_AXIL_XBAR : entity surf.AxiLiteCrossbar
       generic map (
         MASTERS_CONFIG_G   => DDC_CROSSBAR_MASTERS_CONFIG_C,
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => NUM_DDC_G )
       port map (
         axiClk    => axilClk,
         axiClkRst => axilRst,
         -- Slave Slots (Connect to AxiLite Masters)
         sAxiWriteMasters(0) => axilWriteMasters(DDC_INDEX_C),
         sAxiWriteSlaves (0) => axilWriteSlaves (DDC_INDEX_C),
         sAxiReadMasters (0) => axilReadMasters (DDC_INDEX_C),
         sAxiReadSlaves  (0) => axilReadSlaves  (DDC_INDEX_C),
         -- Master Slots (Connect to AXI Slaves)
         mAxiWriteMasters => ddcAxilWriteMasters,
         mAxiWriteSlaves  => ddcAxilWriteSlaves,
         mAxiReadMasters  => ddcAxilReadMasters,
         mAxiReadSlaves   => ddcAxilReadSlaves );
     
     GEN_DDCS : for i in 0 to NUM_DDC_G-1 generate
       U_DESTDIAG : entity l2si.DestDiagControl
         generic map ( TPD_G          => TPD_G,
                       DEFAULT_MASK_G => 2**4 )
         port map ( clk             => timingClk,
                    rst             => timingRst,
                    axilReadMaster  => ddcAxilReadMasters (i),
                    axilReadSlave   => ddcAxilReadSlaves  (i),
                    axilWriteMaster => ddcAxilWriteMasters(i),
                    axilWriteSlave  => ddcAxilWriteSlaves (i),
                    enable          => r.strobe(S0),
                    frame           => frame,
                    dataO           => ddcData        (i),
                    monReset        => seqCountRst,
                    monCount        => seqCount       (i));
     end generate GEN_DDCS;
   end generate GEN_DDC;
   
   GEN_SEQ : for i in 0 to NUM_SEQ_G-1 generate

      status(i).countInvalid(31) <= r.seqInvalid(i);

      U_SeqRst : entity l2si_core.SeqReset
         generic map (
            TPD_G => TPD_G)
         port map (
            clk      => timingClk,
            rst      => timingRst,
            config   => config(i).seqJumpConfig,
            frame    => frame,
            strobe   => r.strobe(S0),
            resetReq => config(i).seqRestart,
            resetO   => seqReset(i));

      U_Jump_i : entity l2si_core.SeqJump
         generic map (
            TPD_G => TPD_G)
         port map (
            clk      => timingClk,
            rst      => timingRst,
            config   => config(i).seqJumpConfig,
            manReset => seqReset(i),
            bcsFault => '0',
            mpsFault => '0',
            mpsClass => (others => '0'),
            jumpEn   => r.strobe(S0),
            jumpReq  => seqJump(i),
            jumpAddr => seqJumpAddr(i));

      U_Seq : entity l2si_core.Sequence
          generic map (
            MON_SUM_G => false )
          port map (
            clkA         => timingClk,
            rstA         => timingRst,
            wrEnA        => config(i).seqWrEn,
            indexA       => gconfig.seqAddr,
            rdStepA      => status(i).seqRdData,
            wrStepA      => gconfig.seqWrData,
            clkB         => timingClk,
            rstB         => timingRst,
            rdEnB        => r.strobe(S0+1),
            waitB        => r.strobe(S0+4),
            acTS         => frame.acTimeSlot,
            acRate       => frame.acRates,
            fixedRate    => frame.fixedRates,
            seqReset     => seqJump (i),
            startAddr    => seqJumpAddr (i),
            seqState     => status(i).seqState,
            seqNotify    => seqNotify (i),
            seqNotifyWr  => seqNotifyValid (i),
            seqNotifyAck => r.ack (i),
            dataO(16 downto 4) => sinkSlv (i),
            dataO( 3 downto 0) => seqData (i),
            monReset     => seqCountRst,
            monCount     => status(i).countRequest);
   end generate;

   --
   --  Replace words in the timing frame.  Skip recalculating the CRC
   --  in the frame, since it will be done on transmission.
   --
   comb : process (timingRst, r, config, timingDataIn, timingAdvance,
                   ddcData, seqReset, seqData, seqNotify, seqNotifyValid,
                   axisSlave) is
      variable v : RegType;
      variable iword : integer;
      variable ibit  : integer;
   begin
      v := r;

      v.advance := timingAdvance;
      v.strobe  := r.strobe(r.strobe'left-1 downto 0) & (timingAdvance and not r.advance);
      v.frame   := timingDataIn & r.frame(r.frame'left downto 16);

      if v.strobe(S0) = '1' then
         v.framel := v.frame;
         --  Require a constant interval between frames
         v.cnt    := (others=>'0');
         v.cntl   := r.cnt;
         if v.cntl/=r.cntl then
           v.seqInvalid := (others=>'1');
         end if;
      end if;

      v.data := timingDataIn;

      if NUM_DDC_G > 0 then
        for i in 0 to NUM_DDC_G-1 loop
          iword := SN - (NUM_SEQ_G + NUM_DDC_G + SEQBITS -1 -i)/SEQBITS;
          ibit  := i mod (16/SEQBITS);
          if r.strobe(iword) = '1' then
            if config(i).seqEnable = '1' then
              v.data((ibit+1)*SEQBITS-1 downto ibit*SEQBITS) := ddcData(i);
            end if;
          end if;
        end loop;
      end if;
      
      for i in 0 to NUM_SEQ_G-1 loop
        iword := SN - (NUM_SEQ_G + SEQBITS -1 -i)/SEQBITS;
        ibit  := i mod (16/SEQBITS);
        if r.strobe(iword) = '1' then
            if config(i).seqEnable = '1' then
              v.data((ibit+1)*SEQBITS-1 downto ibit*SEQBITS) := seqData(i);
            end if;
        end if;
        if seqReset(i)='1' then
          v.seqInvalid(i) := '0';
        end if;
      end loop;

      v.master.tLast := '1';
--      v.master.tKeep := genTKeep(16);

      if axisSlave.tReady = '1' then
         v.master.tValid := '0';
      end if;

      if v.master.tValid = '0' and seqNotifyValid /= 0 then
         v.ack                       := seqNotifyValid;
         ssiSetUserSof (EMAC_AXIS_CONFIG_C, v.master, '1');
         ssiSetUserEofe(EMAC_AXIS_CONFIG_C, v.master, '0');
         v.master.tValid             := '1';
         v.master.tData(31 downto 0) := resize(seqNotifyValid, 16) & XPM_MESSAGE_SEQUENCE_DONE;
         for i in 0 to NUM_SEQ_G-1 loop
            if seqNotifyValid(i) = '1' then
               v.master.tData(12*i+43 downto 12*i+32) := resize(slv(seqNotify(i)), 12);
            end if;
         end loop;
      end if;

      if timingRst = '1' then
         v := REG_INIT_C;
      end if;

      r_in <= v;
   end process comb;

   seq : process (timingClk) is
   begin
      if rising_edge(timingClk) then
         r <= r_in;
      end if;
   end process seq;

end mapping;
