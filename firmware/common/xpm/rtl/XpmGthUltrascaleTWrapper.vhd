-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmGthUltrascaleTWrapper.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2019-10-28
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Interface to sensor link MGT
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


library surf;
use surf.StdRtlPkg.all;
use surf.AxiLitePkg.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;

library l2si_core;
use l2si_core.XpmPkg.all;

library unisim;
use unisim.vcomponents.all;


entity XpmGthUltrascaleTWrapper is
   generic ( GTGCLKRX   : boolean := true;
             USE_IBUFDS : boolean := true );
   port (
      gtTxP            : out sl;
      gtTxN            : out sl;
      gtRxP            : in  sl;
      gtRxN            : in  sl;
      --  Transmit clocking
      devClkP          : in  sl := '0';
      devClkN          : in  sl := '0';
      devClkIn         : in  sl := '0';
      devClkOut        : out sl;
      --  Receive clocking
      timRefClkP       : in  sl;
      timRefClkN       : in  sl;
      --
      stableClk        : in  sl;
      txData           : in  slv(15 downto 0);
      txDataK          : in  slv( 1 downto 0);
      rxData           : out TimingRxType;
      rxClk            : out sl;
      rxRst            : out sl;
      txClk            : out sl;
      txClkIn          : in  sl;
      config           : in  XpmLinkConfigType;
      status           : out XpmLinkStatusType;
      --
      regClk           : in  sl;
      regRst           : in  sl;
      axilReadMaster   : in  AxiLiteReadMasterType;
      axilReadSlave    : out AxiLiteReadSlaveType;
      axilWriteMaster  : in  AxiLiteWriteMasterType;
      axilWriteSlave   : out AxiLiteWriteSlaveType
      );
end XpmGthUltrascaleTWrapper;

architecture rtl of XpmGthUltrascaleTWrapper is

COMPONENT gt_xpm_timing
  PORT (
    gtwiz_userclk_tx_active_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_userclk_rx_active_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_buffbypass_tx_reset_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_buffbypass_tx_start_user_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_buffbypass_tx_done_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_buffbypass_tx_error_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_buffbypass_rx_reset_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_buffbypass_rx_start_user_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_buffbypass_rx_done_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_buffbypass_rx_error_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_reset_clk_freerun_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_reset_all_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_reset_tx_pll_and_datapath_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_reset_tx_datapath_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_reset_rx_pll_and_datapath_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_reset_rx_datapath_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_reset_rx_cdr_stable_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_reset_tx_done_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_reset_rx_done_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtwiz_userdata_tx_in : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    gtwiz_userdata_rx_out : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    gtrefclk00_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtrefclk01_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    qpll0lock_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    qpll0refclklost_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    qpll1lock_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    qpll1outclk_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    qpll1outrefclk_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    qpll1refclklost_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    drpclk_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gthrxn_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gthrxp_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtrefclk0_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    rx8b10ben_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxcommadeten_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxmcommaalignen_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxpcommaalignen_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxusrclk_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxusrclk2_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    tx8b10ben_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    txctrl0_in : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    txctrl1_in : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    txctrl2_in : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    txusrclk_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    txusrclk2_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    cplllock_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gthtxn_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gthtxp_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtpowergood_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxbyteisaligned_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxbyterealign_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxcommadet_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxctrl0_out : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    rxctrl1_out : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    rxctrl2_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    rxctrl3_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    rxdlysresetdone_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxoutclk_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxphaligndone_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxpmaresetdone_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxresetdone_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    txoutclk_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    txpmaresetdone_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    txresetdone_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
  );
END COMPONENT;

  type RegType is record
    clkcnt  : slv(5 downto 0);
    errdet  : sl;
    reset   : sl;
  end record;
  constant REG_INIT_C : RegType := (
    clkcnt  => (others=>'0'),
    errdet  => '0',
    reset   => '0' );

  constant ERR_INTVL : slv(5 downto 0) := (others=>'1');
  
  signal r    : RegType := REG_INIT_C;
  signal rin  : RegType;
  
  signal txCtrl2In  : slv( 7 downto 0);
  signal rxCtrl0Out : slv(15 downto 0);
  signal rxCtrl1Out : slv(15 downto 0);
  signal rxCtrl3Out : slv( 7 downto 0);

  signal txOutClk   : sl;
  signal txUsrClk   : sl;
  signal gtTxRefClk : sl;
  signal gtRxRefClk : sl;
  signal drpClk, idrpClk : sl;
  
  signal rxErrL    : sl;
  signal rxErrS    : sl;
  signal rxErrCnts : slv(15 downto 0);

  signal rxOutClk  : sl;
  signal rxUsrClk  : sl;
  signal rxFifoRst : sl;
  signal rxErrIn   : sl;

  signal rxReset      : sl;
  signal rxResetDone  : sl;
 
  signal rxbypassrst  : sl;
  signal txbypassrst  : sl;

  signal loopback  : slv(2 downto 0);

  signal rxpmaresetdone : sl;
  signal txpmaresetdone : sl;
  signal astatus        : slv(22 downto 0);
  signal xstatus        : XpmLinkStatusType;
  
  type ARegType is record
    axilReadSlave  : AxiLiteReadSlaveType;
    axilWriteSlave : AxiLiteWriteSlaveType;
  end record;

  constant AREG_INIT_C : ARegType := (
    axilReadSlave  => AXI_LITE_READ_SLAVE_INIT_C,
    axilWriteSlave => AXI_LITE_WRITE_SLAVE_INIT_C );

  signal a    : ARegType := AREG_INIT_C;
  signal ain  : ARegType;

  signal rx_error : sl;
  signal rx_cdr_stable : sl;
  signal qpll0lock : sl;
  signal qpll0refclklost : sl;
  signal qpll1lock : sl;
  signal qpll1refclklost : sl;
  signal cplllock : sl;
  signal rxdlysresetdone : sl;
  signal rxphaligndone : sl;
  signal txresetdone : sl;
  signal rxresetdone0 : sl;

  constant DEBUG_C : boolean := false;

  component ila_0
    port ( clk    : in sl;
           probe0 : in slv(255 downto 0) );
  end component;
  
begin

  GEN_DEBUG : if DEBUG_C generate
    U_ILA : ila_0
      port map ( clk       => drpClk,
                 probe0 (0)=> txbypassrst,
                 probe0 (1)=> xstatus.txResetDone,
                 probe0 (2)=> rxbypassrst,
                 probe0 (3)=> xstatus.rxResetDone,
                 probe0 (4)=> xstatus.txReady,
                 probe0 (5)=> rxResetDone,
                 probe0 (6)=> xstatus.txReady,
                 probe0 (7)=> rxpmaresetdone,
                 probe0 (8)=> txpmaresetdone,
                 probe0( 9)=> rx_error,
                 probe0(10)=> rx_cdr_stable,
                 probe0(11)=> qpll1lock,
                 probe0(12)=> qpll1refclklost,
                 probe0(13)=> cplllock,
                 probe0(14)=> rxdlysresetdone,
                 probe0(15)=> rxphaligndone,
                 probe0(16)=> txresetdone,
                 probe0(17)=> rxresetdone0,
                 probe0(18)=> qpll0lock,
                 probe0(19)=> qpll0refclklost,
                 probe0(255 downto 20) => (others=>'0') );
  end generate;    

  rxClk   <= rxUsrClk;
  rxRst   <= rxFifoRst;
  txClk   <= drpClk;  -- reference clk for rx, actually
  status  <= xstatus;
  
  GEN_IBUFDS : if USE_IBUFDS generate
    DEVCLK_IBUFDS_GTE3 : IBUFDS_GTE3
      generic map (
        REFCLK_EN_TX_PATH  => '0',
        REFCLK_HROW_CK_SEL => "01",    -- 2'b01: ODIV2 = Divide-by-2 version of O
        REFCLK_ICNTL_RX    => "00")
      port map (
        I     => devClkP,
        IB    => devClkN,
        CEB   => '0',
        ODIV2 => open,
        O     => gtTxRefClk);
  end generate;

  NO_GEN_IBUFDS : if not USE_IBUFDS generate
    gtTxRefClk <= devClkIn;
  end generate;

  TIMREFCLK_IBUFDS_GTE3 : IBUFDS_GTE3
    generic map (
      REFCLK_EN_TX_PATH  => '0',
      REFCLK_HROW_CK_SEL => "01",    -- 2'b01: ODIV2 = Divide-by-2 version of O
      REFCLK_ICNTL_RX    => "00")
    port map (
      I     => timRefClkP,
      IB    => timRefClkN,
      CEB   => '0',
      ODIV2 => idrpClk,
      O     => gtRxRefClk);

  U_BUFG_GT : BUFG_GT
    port map ( O       => drpClk,
               CE      => '1',
               CEMASK  => '1',
               CLR     => '0',
               CLRMASK => '1',
               DIV     => "000",           -- Divide-by-1
               I       => idrpClk );

  devClkOut  <= gtTxRefClk;
  
  txCtrl2In  <= "000000" & txDataK;
  rxData.decErr <= rxCtrl3Out(1 downto 0);
  rxData.dspErr <= rxCtrl1Out(1 downto 0);
  rxErrIn       <= '1' when (rxCtrl1Out(1 downto 0)/="00" or rxCtrl3Out(1 downto 0)/="00") else '0';
  rxFifoRst  <= not rxResetDone;
  loopback   <= "0" & config.loopback & "0";
  rxReset    <= config.rxReset; -- or r.reset;
  xstatus.rxErr       <= rxErrS;
  xstatus.rxErrCnts   <= rxErrCnts;
  xstatus.rxReady     <= rxResetDone;
    
  U_STATUS : entity surf.SynchronizerOneShotCnt
    generic map ( CNT_WIDTH_G => 16 )
    port map ( dataIn       => rxErrL,
               dataOut      => rxErrS,
               rollOverEn   => '1',
               cntOut       => rxErrCnts,
               wrClk        => rxUsrClk ,
               rdClk        => stableClk );

  U_BUFG  : BUFG_GT
    port map (  I       => rxOutClk,
                CE      => '1',
                CEMASK  => '1',
                CLR     => '0',
                CLRMASK => '1',
                DIV     => "000",
                O       => rxUsrClk );

  --U_TXBUFG  : BUFG_GT
  --  port map (  I       => txOutClk(0),
  --              CE      => '1',
  --              CEMASK  => '1',
  --              CLR     => '0',
  --              CLRMASK => '1',
  --              DIV     => "000",
  --              O       => txUsrClk );
  
  txUsrClk <= txClkIn;
  
  rxErrL   <= rxErrIn;
  rxClk    <= rxUsrClk;
  rxRst    <= rxFifoRst;

  rxData.dataK  <= rxCtrl0Out(1 downto 0);

  U_RstSyncTx : entity surf.RstSync
    port map ( clk      => txUsrClk,
               asyncRst => config.txReset,
               syncRst  => txbypassrst );

  U_RstSyncRx : entity surf.RstSync
    port map ( clk      => rxUsrClk,
               asyncRst => rxReset,
               syncRst  => rxbypassrst );

  U_GthCore : gt_xpm_timing
    PORT MAP (
      gtwiz_userclk_tx_active_in           => "1",
      gtwiz_userclk_rx_active_in           => "1",
      gtwiz_buffbypass_tx_reset_in     (0) => txbypassrst,
      gtwiz_buffbypass_tx_start_user_in    => "0",
      gtwiz_buffbypass_tx_done_out     (0) => xstatus.txResetDone,
      gtwiz_buffbypass_tx_error_out        => open,
      gtwiz_buffbypass_rx_reset_in     (0) => rxbypassrst,
      gtwiz_buffbypass_rx_start_user_in    => "0",
      gtwiz_buffbypass_rx_done_out     (0) => xstatus.rxResetDone,
      gtwiz_buffbypass_rx_error_out    (0) => rx_error,
      gtwiz_reset_clk_freerun_in       (0) => drpClk,
      gtwiz_reset_all_in                   => "0",
      gtwiz_reset_tx_pll_and_datapath_in(0)=> config.txPllReset,
      gtwiz_reset_tx_datapath_in        (0)=> config.txReset,
      gtwiz_reset_rx_pll_and_datapath_in(0)=> config.rxPllReset,
      gtwiz_reset_rx_datapath_in        (0)=> rxReset,
      gtwiz_reset_rx_cdr_stable_out     (0)=> rx_cdr_stable,
      gtwiz_reset_tx_done_out           (0)=> xstatus.txReady,
      gtwiz_reset_rx_done_out           (0)=> rxResetDone,
      gtwiz_userdata_tx_in                 => txData,
      gtwiz_userdata_rx_out                => rxData.data,
      -- QPLL
      gtrefclk00_in                     (0)=> gtRxRefClk,
      gtrefclk01_in                     (0)=> gtRxRefClk,
      qpll0lock_out                     (0)=> qpll0lock,
      qpll0refclklost_out               (0)=> qpll0refclklost,
      qpll1lock_out                     (0)=> qpll1lock,
      qpll1outclk_out                      => open,
      qpll1outrefclk_out                   => open,
      qpll1refclklost_out               (0)=> qpll1refclklost,
      -- CPLL
      gtrefclk0_in                      (0)=> gtTxRefClk,
      drpclk_in                         (0)=> drpClk,
      gthrxn_in                         (0)=> gtRxN,
      gthrxp_in                         (0)=> gtRxP,
--      loopback_in                       (0)=> loopback,
      rx8b10ben_in                         => (others=>'1'),
      rxcommadeten_in                      => (others=>'1'),
      rxmcommaalignen_in                   => (others=>'1'),
      rxpcommaalignen_in                   => (others=>'1'),
      rxusrclk_in                       (0)=> rxUsrClk,
      rxusrclk2_in                      (0)=> rxUsrClk,
      tx8b10ben_in                         => (others=>'1'),
      txctrl0_in                           => (others=>'0'),
      txctrl1_in                           => (others=>'0'),
      txctrl2_in                           => txCtrl2In,
      txusrclk_in                       (0)=> txUsrClk,
      txusrclk2_in                      (0)=> txUsrClk,
      cplllock_out                      (0)=> cplllock,
      gthtxn_out                        (0)=> gtTxN,
      gthtxp_out                        (0)=> gtTxP,
      rxbyteisaligned_out                  => open,
      rxbyterealign_out                    => open,
      rxcommadet_out                       => open,
      rxctrl0_out                          => rxCtrl0Out,
      rxctrl1_out                          => rxCtrl1Out,
      rxctrl2_out                          => open,
      rxctrl3_out                          => rxCtrl3Out,
      rxoutclk_out                      (0)=> rxOutClk,
      rxpmaresetdone_out                (0)=> rxpmaresetdone,
      rxdlysresetdone_out               (0)=> rxdlysresetdone,
      rxphaligndone_out                 (0)=> rxphaligndone,
      rxresetdone_out                   (0)=> rxresetdone0,
      txoutclk_out                      (0)=> txOutClk,
      txpmaresetdone_out                (0)=> txpmaresetdone,
      txresetdone_out                   (0)=> txresetdone
      );

  comb : process ( r, rxResetDone, rxErrIn ) is
    variable v : RegType;
  begin
    v := r;

    if rxErrIn='1' then
      if r.errdet='1' then
        v.reset := '1';
      else
        v.errdet := '1';
      end if;
    end if;

    if r.reset='0' then
      v.clkcnt := r.clkcnt+1;
      if r.clkcnt=ERR_INTVL then
        v.errdet := '0';
        v.clkcnt := (others=>'0');
      end if;
    end if;

    if rxResetDone='0' then
      v := REG_INIT_C;
    end if;
    
    rin <= v;
  end process comb;

  seq : process ( rxUsrClk ) is
  begin
    if rising_edge(rxUsrClk) then
      r <= rin;
    end if;
  end process seq;


  U_Sync_Vector : entity surf.SynchronizerVector
    generic map ( WIDTH_G => astatus'length )
    port map ( clk       => regClk,
               dataIn (0)=> txbypassrst,
               dataIn (1)=> xstatus.txResetDone,
               dataIn (2)=> rxbypassrst,
               dataIn (3)=> xstatus.rxResetDone,
               dataIn (4)=> xstatus.txReady,
               dataIn (5)=> rxResetDone,
               dataIn (6)=> xstatus.txReady,
               dataIn (7)=> rxpmaresetdone,
               dataIn (8)=> txpmaresetdone,
               dataIn (9)=> rxErrIn,
               dataIn(10)=> r.errdet,
               dataIn(11)=> r.reset,
               dataIn(12)=> rx_error,
               dataIn(13)=> rx_cdr_stable,
               dataIn(14)=> qpll1lock,
               dataIn(15)=> qpll1refclklost,
               dataIn(16)=> cplllock,
               dataIn(17)=> rxdlysresetdone,
               dataIn(18)=> rxphaligndone,
               dataIn(19)=> txresetdone,
               dataIn(20)=> rxresetdone0,
               dataIn(21)=> qpll0lock,
               dataIn(22)=> qpll0refclklost,
               dataOut   => astatus );
  
  acomb : process ( a, regRst, axilReadMaster, axilWriteMaster, astatus ) is
    variable v : ARegType;
    variable ep : AxiLiteEndPointType;
  begin
    v := a;

    axiSlaveWaitTxn(ep, axilWriteMaster, axilReadMaster, v.axilWriteSlave, v.axilReadSlave);

    axiSlaveRegisterR(ep, x"00", 0, astatus );
    
    axiSlaveDefault(ep, v.axilWriteSlave, v.axilReadSlave);

    if regRst = '1' then
      v := AREG_INIT_C;
    end if;

    ain <= v;

    axilWriteSlave <= v.axilWriteSlave;
    axilReadSlave  <= v.axilReadSlave;
    
  end process;

  aseq : process ( regClk ) is
  begin
    if rising_edge(regClk) then
      a <= ain;
    end if;
  end process;
  
end rtl;
