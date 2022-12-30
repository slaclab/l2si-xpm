-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmInputSim.vhd
-- Author     : Matt Weaver <weaver@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-08
-- Last update: 2022-12-30
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--   Module to handle simulation of XPM input data
--   LCLS-II Timing Frames (always)
--   Merge receive LCLS-I input stream (option)
--   Clock is either reference clock (no LCLS-I input stream) or
--     186MHz derived from LCLS-I recovered clock (119MHz -> 186MHz)
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
use surf.AxiStreamPkg.all;
use surf.SsiPkg.all;
use surf.AxiLitePkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;
use lcls_timing_core.TPGPkg.all;

library l2si_core;
use l2si_core.CuTimingPkg.all;

library unisim;
use unisim.vcomponents.all;

library l2si;

entity XpmInputSim is
   generic (
      TPD_G               : time    := 1 ns;
      AXIL_BASE_ADDR_G    : slv(31 downto 0);
      SIMULATION_G        : boolean := false;
      CU_RX_ENABLE_INIT_G : boolean := false;
      CU_ASYNC_G          : boolean := false );
   port (
      -- AXI-Lite Interface (axilClk domain)
      axilClk         : in  sl;
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType;
      ----------------------
      -- Top Level Interface
      ----------------------
      timingClk       : in  sl;         -- 186 MHz
      timingClkRst    : in  sl;
      cuTiming        : in  CuTimingType;
      cuTimingV       : in  sl;
      cuDelay         : out slv(17 downto 0);
      --  SC timing input
      usRefClk        : in  sl;         -- 186 MHz
      usRefClkRst     : in  sl;
      --  Cu timing input
      cuRecClk        : in  sl;         -- 119 MHz
      cuRecClkRst     : in  sl;
      cuFiducial      : in  sl;
      cuFiducialIntv  : in  slv(18 downto 0) := (others => '0');
      --
      simClk          : out sl;         -- 186 MHz
      simClkRst       : out sl;
      simLockedN      : out sl;
      simFiducial     : out sl;
      simSync         : out sl;
      simAdvance      : in  sl;
      simStream       : out TimingSerialType);
end XpmInputSim;

architecture mapping of XpmInputSim is

   signal cuClkT     : slv(2 downto 0);
   signal cuRstT     : slv(2 downto 0);
   signal cuSync     : slv(1 downto 0);
   signal mmcmRst    : sl;
   signal cuRxReadyN : sl;
   signal cuBeamCode : slv(7 downto 0);

   signal isimClk, isimRst : sl;
   signal simTx            : TimingPhyType;

   signal tpgConfig : TPGConfigType := TPG_CONFIG_INIT_C;
   signal tpgStatus : TPGStatusType;
   signal status    : TPGStatusType;

   type RegType is record
      config        : TPGConfigType;
      timeStampWrEn : sl;
      pulseIdWrEn   : sl;
      cuDelay       : slv(cuDelay'range);
      cuBeamCode    : slv(7 downto 0);
      cuFiducialErr : sl;
      axiWriteSlave : AxiLiteWriteSlaveType;
      axiReadSlave  : AxiLiteReadSlaveType;
   end record;

   constant REG_INIT_C : RegType := (
      config        => TPG_CONFIG_INIT_C,
      timeStampWrEn => '0',
      pulseIdWrEn   => '0',
      cuDelay       => toSlv(200*800, cuDelay'length),
      cuBeamCode    => toSlv(140, 8),
      cuFiducialErr => '0',
      axiWriteSlave => AXI_LITE_WRITE_SLAVE_INIT_C,
      axiReadSlave  => AXI_LITE_READ_SLAVE_INIT_C);

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

   constant SIM_INDEX_C       : integer := 0;
   constant MMCM0_INDEX_C     : integer := 1;
   constant MMCM1_INDEX_C     : integer := 2;
   constant MMCM2_INDEX_C     : integer := 3;
   constant NUM_AXI_MASTERS_C : integer := 4;

   constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(NUM_AXI_MASTERS_C-1 downto 0) :=
      genAxiLiteConfig(NUM_AXI_MASTERS_C, AXIL_BASE_ADDR_G, 22, 20);

   signal axilWriteMasters : AxiLiteWriteMasterArray(NUM_AXI_MASTERS_C-1 downto 0);
   signal axilWriteSlaves  : AxiLiteWriteSlaveArray (NUM_AXI_MASTERS_C-1 downto 0);
   signal axilReadMasters  : AxiLiteReadMasterArray (NUM_AXI_MASTERS_C-1 downto 0);
   signal axilReadSlaves   : AxiLiteReadSlaveArray (NUM_AXI_MASTERS_C-1 downto 0);

   type CuRegType is record
      fiducialErr  : sl;
      fiducialErrL : sl;
      fiducialTest : slv(cuFiducialIntv'range);
      cuRxReady    : sl;
   end record;

   constant CUREG_INIT_C : CuRegType := (
      fiducialErr  => '0',
      fiducialErrL => '0',
      fiducialTest => (others => '0'),
      cuRxReady    => '0' );

   signal cr   : CuRegType := CUREG_INIT_C;
   signal crin : CuRegType;

   signal cuFiducialIntvS : slv(cuFiducialIntv'range);
   signal cuFiducialErrS  : sl;
   signal phaseReset      : sl;

begin

  tpgConfig.FixedRateDivisors <= (x"00000",
                                  x"00000",
                                  x"00000",
                                  x"00001",                                    -- 929 kHz
                                  x"0000D",                                    -- 71.4kHz
                                  x"0005B",                                    -- 10.2kHz
                                  x"0038E",                                    -- 1.02kHz
                                  x"0238C",                                    -- 102 Hz
                                  x"16378",                                    -- 10.2Hz
                                  x"DE2B0");                                   -- 1.02Hz


   isimClk  <= timingClk;
   isimRst  <= timingClkRst;

   GEN_CU_RX_ENABLE : if CU_RX_ENABLE_INIT_G = true generate
      simClk     <= cuClkT(2);
      simClkRst  <= not cr.cuRxReady;
      simLockedN <= cuRstT(2);
      simSync    <= mmcmRst;
   end generate;

   GEN_CU_RX_DISABLE : if CU_RX_ENABLE_INIT_G = false generate
      simClk     <= usRefClk;
      simClkRst  <= usRefClkRst;
      simLockedN <= usRefClkRst;
      simSync    <= '0';
   end generate;

   --------------------------
   -- AXI-Lite: Crossbar Core
   --------------------------  
   U_XBAR : entity surf.AxiLiteCrossbar
      generic map (
         TPD_G              => TPD_G,
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => AXI_CROSSBAR_MASTERS_CONFIG_C'length,
         MASTERS_CONFIG_G   => AXI_CROSSBAR_MASTERS_CONFIG_C)
      port map (
         axiClk              => axilClk,
         axiClkRst           => axilRst,
         sAxiWriteMasters(0) => axilWriteMaster,
         sAxiWriteSlaves(0)  => axilWriteSlave,
         sAxiReadMasters(0)  => axilReadMaster,
         sAxiReadSlaves(0)   => axilReadSlave,
         mAxiWriteMasters    => axilWriteMasters,
         mAxiWriteSlaves     => axilWriteSlaves,
         mAxiReadMasters     => axilReadMasters,
         mAxiReadSlaves      => axilReadSlaves);

   --
   --  How to insure each of these lock at a fixed phase with respect to 71kHz
   --  strobe?
   --
   --  Measure phase offset at 71kHz and shift to a known value.
   --
   GEN_SYNC: if not CU_ASYNC_G generate
     phaseReset <= cuFiducial;
   end generate GEN_SYNC;
   GEN_ASYNC: if CU_ASYNC_G generate
     phaseReset <= cuFiducial and not cr.cuRxReady;
   end generate GEN_ASYNC;
   
   BaseEnableDivider : entity lcls_timing_core.Divider
      generic map (
         TPD_G => TPD_G,
         Width => 11)
      port map (
         sysClk   => cuRecClk,
         sysReset => phaseReset,
         enable   => cr.cuRxReady,
         clear    => '0',
         divisor  => toSlv(1666, 11),
         trigO    => mmcmRst);

   cuRxReadyN <= not cr.cuRxReady;

   U_MMCM0 : entity l2si.MmcmPhaseLock
      generic map (
         TPD_G             => TPD_G,
         CLKIN_PERIOD_G    => 8.4,      -- ClkIn  = 119MHz
         CLKOUT_DIVIDE_F_G => 17.0,     -- ClkOut =  70MHz
         CLKFBOUT_MULT_F_G => 10.0,     -- VCO    = 1190MHz
         NUM_LOCKS_G       => 1,
         SIMULATION_G      => SIMULATION_G)
      port map (
         clkIn           => cuRecClk,
         rstIn           => cuRxReadyN,
         clkSync         => cuClkT(0),
         syncIn          => mmcmRst,
         clkOut          => cuClkT(0),
         rstOut          => cuRstT(0),
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilWriteMaster => axilWriteMasters(MMCM0_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (MMCM0_INDEX_C),
         axilReadMaster  => axilReadMasters (MMCM0_INDEX_C),
         axilReadSlave   => axilReadSlaves (MMCM0_INDEX_C));

   U_MMCM1 : entity l2si.MmcmPhaseLock
      generic map (
         TPD_G             => TPD_G,
         CLKIN_PERIOD_G    => 14.286,   -- ClkIn  =  70MHz
         CLKOUT_DIVIDE_F_G => 7.0,      -- ClkOut = 130MHz
         CLKFBOUT_MULT_F_G => 13.0,     -- VCO    = 910MHz
         NUM_LOCKS_G       => 1,
         SIMULATION_G      => SIMULATION_G)
      port map (
         clkIn           => cuClkT(0),
         rstIn           => cuRstT(0),
         clkSync         => cuClkT(1),
         syncIn          => mmcmRst,
         clkOut          => cuClkT(1),
         rstOut          => cuRstT(1),
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilWriteMaster => axilWriteMasters(MMCM1_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (MMCM1_INDEX_C),
         axilReadMaster  => axilReadMasters (MMCM1_INDEX_C),
         axilReadSlave   => axilReadSlaves (MMCM1_INDEX_C));

   U_MMCM2 : entity l2si.MmcmPhaseLock
      generic map (
         TPD_G             => TPD_G,
         CLKIN_PERIOD_G    => 7.692,    -- ClkIn  = 130MHz
         CLKOUT_DIVIDE_F_G => 7.0,      -- ClkOut = 185.7MHz
         CLKFBOUT_MULT_F_G => 10.0,     -- VCO    = 1300MHz
         NUM_LOCKS_G       => 1,
         SIMULATION_G      => SIMULATION_G)
      port map (
         clkIn           => cuClkT(1),
         rstIn           => cuRstT(1),
         clkSync         => cuClkT(2),
         syncIn          => mmcmRst,
         clkOut          => cuClkT(2),
         rstOut          => cuRstT(2),
         axilClk         => axilClk,
         axilRst         => axilRst,
         axilWriteMaster => axilWriteMasters(MMCM2_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (MMCM2_INDEX_C),
         axilReadMaster  => axilReadMasters (MMCM2_INDEX_C),
         axilReadSlave   => axilReadSlaves (MMCM2_INDEX_C));

   U_CuSync0 : entity surf.RstSync
      generic map (
         TPD_G => TPD_G)
      port map (
         clk      => cuClkT(0),
         asyncRst => cuFiducial,
         syncRst  => cuSync(0));

   U_CuSync1 : entity surf.RstSync
      generic map (
         TPD_G => TPD_G)
      port map (
         clk      => cuClkT(1),
         asyncRst => cuSync(0),
         syncRst  => cuSync(1));

   SC_SIM : if CU_RX_ENABLE_INIT_G = false generate
      -- simulated LCLS2 stream
      U_UsSim : entity lcls_timing_core.TPGMini
         generic map (
            TPD_G       => TPD_G,
            NARRAYSBSA  => 0,
            STREAM_INTF => true)
         port map (
            statusO    => tpgStatus,
            configI    => tpgConfig,
            --
            txClk      => isimClk,
            txRst      => isimRst,
            txRdy      => '1',
            streams(0) => simStream,
            streamIds  => open,
            advance(0) => simAdvance,
            fiducial   => simFiducial);
   end generate;

   SC_GEN : if CU_RX_ENABLE_INIT_G = true generate
      -- translated LCLS2 stream
      U_XTPG : entity l2si.XpmStreamFromCu
         generic map (
           TPD_G   => TPD_G,
           ASYNC_G => CU_ASYNC_G )
         port map (
            statusO   => tpgStatus,
            configI   => tpgConfig,
            beamCode  => cuBeamCode,
            --
            txClk     => isimClk,
            txRst     => isimRst,
            txRdy     => '1',
            cuTiming  => cuTiming,
            cuTimingV => cuTimingV,
            stream    => simStream,
            advance   => simAdvance,
            fiducial  => simFiducial);
   end generate;

   U_Sync_TimeStamp : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => 64)
      port map (
         clk     => isimClk,
         dataIn  => r.config.timeStamp,
         dataOut => tpgConfig.timeStamp);

   U_Sync_TimeStampWr : entity surf.Synchronizer
      generic map (
         TPD_G => TPD_G)
      port map (
         clk     => isimClk,
         dataIn  => r.config.timeStampWrEn,
         dataOut => tpgConfig.timeStampWrEn);

   U_Sync_PulseId : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => 64)
      port map (
         clk     => isimClk,
         dataIn  => r.config.pulseId,
         dataOut => tpgConfig.pulseId);

   U_Sync_PulseIdWr : entity surf.Synchronizer
      generic map (
         TPD_G => TPD_G)
      port map (
         clk     => isimClk,
         dataIn  => r.config.pulseIdWrEn,
         dataOut => tpgConfig.pulseIdWrEn);

   U_Sync_TimeStampRd : entity surf.SynchronizerFifo
      generic map (
         TPD_G        => TPD_G,
         DATA_WIDTH_G => 64)
      port map (
         wr_clk => isimClk,
         din    => tpgStatus.timeStamp,
         rd_clk => axilClk,
         dout   => status.timeStamp);

   U_Sync_PulseIdRd : entity surf.SynchronizerFifo
      generic map (
         TPD_G        => TPD_G,
         DATA_WIDTH_G => 64)
      port map (
         wr_clk => isimClk,
         din    => tpgStatus.pulseId,
         rd_clk => axilClk,
         dout   => status.pulseId);

   U_Sync_CuDelay : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => cuDelay'length)
      port map (
         clk     => isimClk,
         dataIn  => r.cuDelay,
         dataOut => cuDelay);

   U_Sync_CuBeamCode : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => cuBeamCode'length)
      port map (
         clk     => isimClk,
         dataIn  => r.cuBeamCode,
         dataOut => cuBeamCode);

   U_Sync_CuFiducialIntv : entity surf.SynchronizerVector
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => cuFiducialIntv'length)
      port map (
         clk     => axilClk,
         dataIn  => cuFiducialIntv,
         dataOut => cuFiducialIntvS);

   U_Sync_CuFiducialErrL : entity surf.Synchronizer
      generic map (
         TPD_G => TPD_G)
      port map (
         clk     => axilClk,
         dataIn  => cr.fiducialErrL,
         dataOut => cuFiducialErrS);

   ccomb : process (cr, cuFiducial, cuFiducialIntv, cuRecClkRst) is
      variable v : CuRegType;
      variable d : slv(cuFiducialIntv'range);
   begin
      v := cr;

      if cr.fiducialTest = cuFiducialIntv then
         v.fiducialErr := '0';
      elsif cr.fiducialTest < cuFiducialIntv then
         v.fiducialTest := cr.fiducialTest + 1666;
      end if;

      if cuFiducial = '1' then
         v.cuRxReady    := '1';
         v.fiducialErrL := cr.fiducialErr;
         v.fiducialErr  := '1';
         v.fiducialTest := (others => '0');
      end if;

      if cuRecClkRst = '1' then
         v := CUREG_INIT_C;
      end if;

      crin <= v;
   end process ccomb;

   cseq : process (cuRecClk) is
   begin
      if rising_edge(cuRecClk) then
         cr <= crin after TPD_G;
      end if;
   end process cseq;

   comb : process (axilReadMasters, axilRst, axilWriteMasters, cuFiducialErrS, cuFiducialIntvS, r, status) is
      variable v        : RegType;
      variable ep       : AxiLiteEndpointType;
      variable clearErr : sl;
   begin
      v                      := r;
      v.timeStampWrEn        := '0';
      v.pulseIdWrEn          := '0';
      v.config.timeStampWrEn := r.timeStampWrEn;
      v.config.pulseIdWrEn   := r.pulseIdWrEn;

      axiSlaveWaitTxn(ep, axilWriteMasters(SIM_INDEX_C), axilReadMasters(SIM_INDEX_C), v.axiWriteSlave, v.axiReadSlave);

      axiSlaveRegister(ep, x"00", 0, v.config.timeStamp(31 downto 0));
      axiSlaveRegister(ep, x"04", 0, v.config.timeStamp(63 downto 32));
      axiSlaveRegister(ep, x"08", 0, v.config.pulseId(31 downto 0));
      axiSlaveRegister(ep, x"0C", 0, v.config.pulseId(63 downto 32));
      axiWrDetect(ep, x"04", v.timeStampWrEn);
      axiWrDetect(ep, x"0C", v.pulseIdWrEn);

      axiSlaveRegisterR(ep, x"00", 0, status.timeStamp(31 downto 0));
      axiSlaveRegisterR(ep, x"04", 0, status.timeStamp(63 downto 32));
      axiSlaveRegisterR(ep, x"08", 0, status.pulseId(31 downto 0));
      axiSlaveRegisterR(ep, x"0C", 0, status.pulseId(63 downto 32));

      axiSlaveRegister (ep, x"10", 0, v.cuDelay);
      if SIMULATION_G then
         v.cuDelay := toSlv(20000, cuDelay'length);
      end if;

      axiSlaveRegister (ep, x"14", 0, v.cuBeamCode);
      axiSlaveRegisterR(ep, x"18", 0, cuFiducialIntvS);
      axiSlaveRegisterR(ep, x"18", 31, r.cuFiducialErr);
      clearErr := '0';
      axiWrDetect(ep, x"18", clearErr);
      if clearErr = '1' then
         v.cuFiducialErr := '0';
      elsif cuFiducialErrS = '1' then
         v.cuFiducialErr := '1';
      end if;

      axiSlaveDefault(ep, v.axiWriteSlave, v.axiReadSlave);

      axilWriteSlaves(SIM_INDEX_C) <= r.axiWriteSlave;
      axilReadSlaves (SIM_INDEX_C) <= r.axiReadSlave;

      if axilRst = '1' then
         v := REG_INIT_C;
      end if;

      rin <= v;

   end process comb;

   seq : process (axilClk) is
   begin
      if rising_edge(axilClk) then
         r <= rin after TPD_G;
      end if;
   end process seq;

end mapping;
