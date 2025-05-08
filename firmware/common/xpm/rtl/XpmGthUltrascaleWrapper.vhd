-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : XpmGthUltrascaleWrapper.vhd
-- Author     : Matt Weaver
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-12-14
-- Last update: 2025-05-08
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

library l2si_core;
use l2si_core.XpmPkg.all;

library unisim;
use unisim.vcomponents.all;


entity XpmGthUltrascaleWrapper is
   generic ( GTGCLKRX         : boolean := true;
             NLINKS_G         : integer range 1 to 7:= 7;
             USE_IBUFDS       : boolean := true;
             AXIL_BASE_ADDR_G : slv(31 downto 0) := (others => '0');
             DEBUG_G          : boolean := true
       );
   port (
      gtTxP            : out slv(NLINKS_G-1 downto 0);
      gtTxN            : out slv(NLINKS_G-1 downto 0);
      gtRxP            : in  slv(NLINKS_G-1 downto 0);
      gtRxN            : in  slv(NLINKS_G-1 downto 0);
      devClkP          : in  sl := '0';
      devClkN          : in  sl := '0';
      devClkIn         : in  sl := '0';
      devClkOut        : out sl;
      devClkBuf        : out sl;
      stableClk        : in  sl;
      txData           : in  Slv16Array(NLINKS_G-1 downto 0);
      txDataK          : in  Slv2Array (NLINKS_G-1 downto 0);
      rxData           : out Slv16Array(NLINKS_G-1 downto 0);
      rxDataK          : out Slv2Array (NLINKS_G-1 downto 0);
      rxClk            : out slv       (NLINKS_G-1 downto 0);
      rxRst            : out slv       (NLINKS_G-1 downto 0);
      rxErr            : out slv       (NLINKS_G-1 downto 0);
      txOutClk         : out slv       (NLINKS_G-1 downto 0);
      txClk            : out sl;
      txClkIn          : in  sl;
      txClkRst         : in  sl;
      config           : in  XpmLinkConfigArray(NLINKS_G-1 downto 0);
      status           : out XpmLinkStatusArray(NLINKS_G-1 downto 0);
            
      axilRst          : in sl;
      axilReadMaster   : in  AxiLiteReadMasterType;
      axilReadSlave    : out AxiLiteReadSlaveType;
      axilWriteMaster  : in  AxiLiteWriteMasterType;
      axilWriteSlave   : out AxiLiteWriteSlaveType
      
      );
end XpmGthUltrascaleWrapper;

architecture rtl of XpmGthUltrascaleWrapper is

COMPONENT gt_dslink_ss_nophase_amc0
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
--    cpllrefclksel_in : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
    drpclk_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
--    gtgrefclk_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gthrxn_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gthrxp_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtrefclk0_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    loopback_in : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
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
    gthtxn_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gthtxp_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxbyteisaligned_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxbyterealign_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxcommadet_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxctrl0_out : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    rxctrl1_out : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    rxctrl2_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    rxctrl3_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    rxoutclk_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxpmaresetdone_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    txoutclk_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    txpmaresetdone_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    drpdo_out : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    drprdy_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    drpaddr_in : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    drpdi_in : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    drpen_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    drpwe_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    
    rxcdrlock_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    gtpowergood_out : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    eyescanreset_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    rxpmareset_in : IN STD_LOGIC_VECTOR(0 DOWNTO 0)
  );
END COMPONENT;

  type RegType is record
    clkcnt  : slv(5 downto 0);
    errdet  : sl;
    reset   : sl;
    gthWrdCnt : slv(31 downto 0);
    gthErrCnt : slv(31 downto 0);
  end record;
  constant REG_INIT_C : RegType := (
    clkcnt  => (others=>'0'),
    errdet  => '0',
    reset   => '0',
    gthWrdCnt => (others => '0'),
    gthErrCnt => (others => '0')
  );
  type RegTypeArray is array(natural range<>) of RegType;

  constant ERR_INTVL : slv(5 downto 0) := (others=>'1');
  
  signal r    : RegTypeArray(NLINKS_G-1 downto 0) := (others=>REG_INIT_C);
  signal rin  : RegTypeArray(NLINKS_G-1 downto 0);
  
  signal txCtrl2In  : Slv8Array (NLINKS_G-1 downto 0);
  signal rxCtrl0Out : Slv16Array(NLINKS_G-1 downto 0);
  signal rxCtrl1Out : Slv16Array(NLINKS_G-1 downto 0);
  signal rxCtrl3Out : Slv8Array (NLINKS_G-1 downto 0);

  signal txOutClkO  : slv(NLINKS_G-1 downto 0);
  signal txUsrClk   : slv(NLINKS_G-1 downto 0);
  signal txFifoRst  : slv(NLINKS_G-1 downto 0);
  signal gtRefClk   : sl;
  signal gRefClk    : sl;

  signal rxErrL    : slv(NLINKS_G-1 downto 0);
  signal rxErrS    : slv(NLINKS_G-1 downto 0);
  signal rxErrCnts : Slv16Array(NLINKS_G-1 downto 0);

  signal rxOutClk  : slv(NLINKS_G-1 downto 0);
  signal rxUsrClk  : slv(NLINKS_G-1 downto 0);
  signal rxFifoRst : slv(NLINKS_G-1 downto 0);
  signal txDataS      : Slv18Array(NLINKS_G-1 downto 0);

  --  Signals also on ILA (set to constant length)
  signal txbypassrst  : slv(7 downto 0);
  signal txbypassdone : slv(7 downto 0);
  signal txbypasserr  : slv(7 downto 0);
  signal txpllreset   : slv(7 downto 0);
  signal txUsrRst     : slv(7 downto 0);
  signal txReady      : slv(7 downto 0);
  signal txpmarstdone : slv(7 downto 0);
  signal rxbypassrst  : slv(7 downto 0);
  signal rxbypassdone : slv(7 downto 0);
  signal rxpllreset   : slv(7 downto 0);
  signal rxReset      : slv(7 downto 0);
  signal rxResetDone  : slv(7 downto 0);
  signal rxpmarst     : slv(7 downto 0);
  signal rxpmarstdone : slv(7 downto 0);
  signal rxcdrlock    : slv(7 downto 0);
  signal rxErrIn      : slv(7 downto 0);

  signal txResetDone  : slv(7 downto 0);
  --
  
  signal loopback  : Slv3Array(NLINKS_G-1 downto 0);

  signal drpdo    : Slv16Array(NLINKS_G-1 downto 0);
  signal drprdy   : slv(NLINKS_G-1 downto 0);
  signal drpaddr  : Slv9Array(NLINKS_G-1 downto 0);
  signal drpdi    : Slv16Array(NLINKS_G-1 downto 0);
  signal drpen    : slv(NLINKS_G-1 downto 0);
  signal drpwe    : slv(NLINKS_G-1 downto 0);
  
  constant LINK0_INDEX_C : integer := 0;
  constant LINK1_INDEX_C : integer := 1;
  constant LINK2_INDEX_C : integer := 2;
  constant LINK3_INDEX_C : integer := 3;
  constant LINK4_INDEX_C : integer := 4;
  constant LINK5_INDEX_C : integer := 5;
  constant LINK6_INDEX_C : integer := 6;
  constant LINK7_INDEX_C : integer := 7;
  constant LINK8_INDEX_C : integer := 8;
  
  constant AXI_XBAR_CONFIG_C : AxiLiteCrossbarMasterConfigArray(8 downto 0) := (
     LINK0_INDEX_C   => (baseAddr     => AXIL_BASE_ADDR_G,
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     LINK1_INDEX_C  => (baseAddr     => AXIL_BASE_ADDR_G + X"10000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     LINK2_INDEX_C  => (baseAddr     => AXIL_BASE_ADDR_G + X"20000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     LINK3_INDEX_C  => (baseAddr     => AXIL_BASE_ADDR_G + X"30000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     LINK4_INDEX_C   => (baseAddr     => AXIL_BASE_ADDR_G + X"40000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     LINK5_INDEX_C  => (baseAddr     => AXIL_BASE_ADDR_G + X"50000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     LINK6_INDEX_C  => (baseAddr     => AXIL_BASE_ADDR_G + X"60000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     LINK7_INDEX_C  => (baseAddr     => AXIL_BASE_ADDR_G + X"70000",
                       addrBits     => 16,
                       connectivity => X"FFFF"),
     LINK8_INDEX_C  => (baseAddr     => AXIL_BASE_ADDR_G + X"80000",
                       addrBits     => 16,
                       connectivity => X"FFFF") );

  signal axilReadMasters  : AxiLiteReadMasterArray (AXI_XBAR_CONFIG_C'range);
  signal axilReadSlaves   : AxiLiteReadSlaveArray (AXI_XBAR_CONFIG_C'range) := (others=>AXI_LITE_READ_SLAVE_EMPTY_OK_C);
  signal axilWriteMasters : AxiLiteWriteMasterArray(AXI_XBAR_CONFIG_C'range);
  signal axilWriteSlaves  : AxiLiteWriteSlaveArray (AXI_XBAR_CONFIG_C'range) := (others=>AXI_LITE_WRITE_SLAVE_EMPTY_OK_C);
   
  constant TX_SYNC_C : boolean := true;

  component ila_0
    port ( clk    : sl;
           probe0 : slv(255 downto 0) );
  end component;
  
begin

  rxClk    <= rxUsrClk;
  rxErr    <= rxErrL;
  txClk    <= txUsrClk(0);
  txOutClk <= txUsrClk;
  
  U_XBAR : entity surf.AxiLiteCrossbar
      generic map (
         DEC_ERROR_RESP_G   => AXI_RESP_DECERR_C,
         NUM_SLAVE_SLOTS_G  => 1,
         NUM_MASTER_SLOTS_G => AXI_XBAR_CONFIG_C'length,
         MASTERS_CONFIG_G   => AXI_XBAR_CONFIG_C)
      port map (
         axiClk              => stableClk,
         axiClkRst           => axilRst,
         sAxiWriteMasters(0) => axilWriteMaster,
         sAxiWriteSlaves(0)  => axilWriteSlave,
         sAxiReadMasters(0)  => axilReadMaster,
         sAxiReadSlaves (0)  => axilReadSlave,
         mAxiWriteMasters    => axilWriteMasters,
         mAxiWriteSlaves     => axilWriteSlaves,
         mAxiReadMasters     => axilReadMasters,
         mAxiReadSlaves      => axilReadSlaves);
         
  GEN_IBUFDS : if USE_IBUFDS generate
    DEVCLK_IBUFDS_GTE3 : IBUFDS_GTE3
      generic map (
        REFCLK_EN_TX_PATH  => '0',
        REFCLK_HROW_CK_SEL => "00",    -- 2'b01: ODIV2 = Divide-by-2 version of O
        REFCLK_ICNTL_RX    => "00")
      port map (
        I     => devClkP,
        IB    => devClkN,
        CEB   => '0',
        ODIV2 => gRefClk,
        O     => gtRefClk);
  end generate;

  NO_GEN_IBUFDS : if not USE_IBUFDS generate
    gtRefClk <= devClkIn;
  end generate;

  devClkOut  <= gtRefClk;
  devClkBuf  <= gRefClk;

  GEN_CTRL : for i in 0 to NLINKS_G-1 generate
    txCtrl2In (i) <= "000000" & txDataS(i)(17 downto 16);
    rxErrIn   (i) <= '0' when (rxCtrl1Out(i)(1 downto 0)="00" and rxCtrl3Out(i)(1 downto 0)="00") else '1';
    txFifoRst (i) <= not txbypassdone(i);
    rxFifoRst (i) <= not rxResetDone(i);
    loopback  (i) <= "0" & config(i).loopback & "0";
    status    (i).rxErr       <= rxErrS(i);
    status    (i).rxErrCnts   <= rxErrCnts(i);
    status    (i).rxReady     <= rxResetDone(i);
    rxReset   (i) <= config(i).rxReset or r(i).reset;
    txResetDone(i) <= txbypassdone(i) and not txbypasserr(i);
    
    U_STATUS : entity surf.SynchronizerOneShotCnt
      generic map ( CNT_WIDTH_G => 16 )
      port map ( dataIn       => rxErrL(i),
                 dataOut      => rxErrS(i),
                 rollOverEn   => '1',
                 cntOut       => rxErrCnts(i),
                 wrClk        => rxUsrClk (i),
                 rdClk        => stableClk );

    U_TXSTATUS : entity surf.SynchronizerVector
      generic map ( WIDTH_G => 2 )
      port map ( clk         => stableClk,
                 dataIn (0)  => txResetDone(i),
                 dataIn (1)  => txReady    (i),
                 dataOut(0)  => status(i).txResetDone,
                 dataOut(1)  => status(i).txReady );
                 
    U_BUFG  : BUFG_GT
      port map (  I       => rxOutClk(i),
                  CE      => '1',
                  CEMASK  => '1',
                  CLR     => '0',
                  CLRMASK => '1',
                  DIV     => "000",
                  O       => rxUsrClk(i) );

    GEN_TXSYNC : if TX_SYNC_C generate
      U_TXBUFG  : BUFG_GT
        port map (  I       => txOutClkO(i),
                    CE      => '1',
                    CEMASK  => '1',
                    CLR     => '0',
                    CLRMASK => '1',
                    DIV     => "000",
                    O       => txUsrClk(i) );
      U_TxSync : entity surf.SynchronizerFifo
        generic map ( DATA_WIDTH_G => 18,
                      ADDR_WIDTH_G => 4 )  -- avoid full thresholds
        port map ( rst               => txFifoRst(i),
                   wr_clk            => txClkIn,
                   din(17 downto 16) => txDataK (i),
                   din(15 downto  0) => txData  (i),
                   rd_clk            => txUsrClk(i),
                   dout              => txDataS (i) );
    end generate;

    NO_GEN_TXSYNC : if not TX_SYNC_C generate
      txUsrClk(i) <= txClkIn;
      txDataS (i) <= txDataK(i) & txData(i);
    end generate;
    
    txUsrRst(i) <= txClkRst or config(i).txReset;
    
    rxErrL (i)  <= rxErrIn(i);
    rxClk  (i)  <= rxUsrClk(i);
    rxRst  (i)  <= rxFifoRst(i);
    rxDataK(i)  <= rxCtrl0Out(i)(1 downto 0);

    U_RstSyncTx : entity surf.RstSync
      port map ( clk      => txUsrClk(i),
                 asyncRst => txUsrRst(i),
                 syncRst  => txbypassrst(i) );

    U_RstSyncRx : entity surf.RstSync
      port map ( clk      => rxUsrClk(i),
                 asyncRst => rxReset(i),
                 syncRst  => rxbypassrst(i) );
                     
   U_Axi2Drp: entity surf.AxiLiteToDrp 
      generic map(
          ADDR_WIDTH_G   => 9
      )
      port map(
          -- AXI-Lite Port
          axilClk         => stableClk,
          axilRst         => axilRst,
          axilReadMaster  => axilReadMasters(i),
          axilReadSlave   => axilReadSlaves(i),
          axilWriteMaster => axilWriteMasters(i),
          axilWriteSlave  => axilWriteSlaves(i),
          
          -- DRP Interface
          drpClk          => stableClk,
          drpRst          => axilRst,
          drpRdy          => drprdy(i),
          drpEn           => drpen(i),
          drpWe           => drpwe(i),
          drpUsrRst       => open,
          drpAddr         => drpaddr(i),
          drpDi           => drpdi(i),
          drpDo           => drpdo(i)
      );

  
  
    U_GthCore : gt_dslink_ss_nophase_amc0 -- 1 RTM link
      PORT MAP (
        gtwiz_userclk_tx_active_in           => "1",
        gtwiz_userclk_rx_active_in           => "1",
        gtwiz_buffbypass_tx_reset_in     (0) => txbypassrst(i),
        gtwiz_buffbypass_tx_start_user_in    => "0",
        gtwiz_buffbypass_tx_done_out     (0) => txbypassdone(i),
        gtwiz_buffbypass_tx_error_out    (0) => txbypasserr(i),
        gtwiz_buffbypass_rx_reset_in     (0) => rxbypassrst(i),
        gtwiz_buffbypass_rx_start_user_in    => "0",
        gtwiz_buffbypass_rx_done_out     (0) => rxbypassdone(i),
        gtwiz_buffbypass_rx_error_out        => open,  -- Might need this
        gtwiz_reset_clk_freerun_in(0)        => stableClk,
        gtwiz_reset_all_in                   => "0",
        gtwiz_reset_tx_pll_and_datapath_in(0)=> txpllreset(i),
        gtwiz_reset_tx_datapath_in        (0)=> txUsrRst(i),
        gtwiz_reset_rx_pll_and_datapath_in(0)=> rxpllreset(i),
        gtwiz_reset_rx_datapath_in        (0)=> rxReset(i),
        gtwiz_reset_rx_cdr_stable_out        => open,
        gtwiz_reset_tx_done_out           (0)=> txReady(i),
        gtwiz_reset_rx_done_out           (0)=> rxResetDone(i),
        gtwiz_userdata_tx_in                 => txDataS(i)(15 downto 0),
        gtwiz_userdata_rx_out                => rxData(i),
        -- CPLL
        drpclk_in                         (0)=> stableClk,
        gthrxn_in                         (0)=> gtRxN(i),
        gthrxp_in                         (0)=> gtRxP(i),
        gtrefclk0_in                      (0)=> gtRefClk,
        loopback_in                          => loopback(i),
        rx8b10ben_in                         => (others=>'1'),
        rxcommadeten_in                      => (others=>'1'),
        rxmcommaalignen_in                   => (others=>'1'),
        rxpcommaalignen_in                   => (others=>'1'),
        rxusrclk_in                       (0)=> rxUsrClk(i),
        rxusrclk2_in                      (0)=> rxUsrClk(i),
        tx8b10ben_in                         => (others=>'1'),
        txctrl0_in                           => (others=>'0'),
        txctrl1_in                           => (others=>'0'),
        txctrl2_in                           => txCtrl2In(i),
        txusrclk_in                       (0)=> txUsrClk(i),
        txusrclk2_in                      (0)=> txUsrClk(i),
        gthtxn_out                        (0)=> gtTxN(i),
        gthtxp_out                        (0)=> gtTxP(i),
        rxbyteisaligned_out                  => open,
        rxbyterealign_out                    => open,
        rxcommadet_out                       => open,
        rxctrl0_out                          => rxCtrl0Out(i),
        rxctrl1_out                          => rxCtrl1Out(i),
        rxctrl2_out                          => open,
        rxctrl3_out                          => rxCtrl3Out(i),
        rxoutclk_out                      (0)=> rxOutClk(i),
        rxpmaresetdone_out                (0)=> rxpmarstdone(i),
        txoutclk_out                      (0)=> txOutClkO(i),
        txpmaresetdone_out                (0)=> txpmarstdone(i),
        drpdo_out                            => drpdo(i),
        drprdy_out                        (0)=> drprdy(i),
        drpaddr_in                           => drpaddr(i),
        drpdi_in                             => drpdi(i),
        drpen_in                          (0)=> drpen(i),
        drpwe_in                          (0)=> drpwe(i),
        rxcdrlock_out                     (0)=> rxcdrlock(i),
        gtpowergood_out                   (0)=> status(i).gtpowergood,
        eyescanreset_in                   (0)=> config(i).eyescanrst,
        rxpmareset_in                     (0)=> rxpmarst(i) 
        );

    txpllreset(i) <= config(i).txPllReset;
    rxpllreset(i) <= config(i).rxPllReset;
    rxpmarst(i)   <= config(i).rxpmarst;
    
    status(i).rxpmarstdone  <= rxpmarstdone(i);
    status(i).txpmarstdone  <= txpmarstdone(i);
    status(i).rxResetDone   <= rxbypassdone(i);
    status(i).rxcdrlock     <= rxcdrlock(i);
    status(i).rxGTHWordCnts <= r(i).gthWrdCnt;
    status(i).rxGTHErrCnts  <= r(i).gthErrCnt;
    
    comb : process ( r, rxResetDone, rxErrIn, config ) is
      variable v : RegType;
    begin
      v := r(i);

      if rxErrIn(i)='1' then
        if r(i).errdet='1' then
          v.reset := '1';
        else
          v.errdet := '1';
        end if;
      end if;

      if r(i).reset='0' then
        v.clkcnt := r(i).clkcnt+1;
        if r(i).clkcnt=ERR_INTVL then
          v.errdet := '0';
          v.clkcnt := (others=>'0');
        end if;
      end if;
      
      v.gthWrdCnt := r(i).gthWrdCnt + 1;
      if rxErrIn(i) = '1' then
          v.gthErrCnt := r(i).gthErrCnt + 1;
      end if;
      
      if config(i).rstGthCnter = '1' then
          v.gthWrdCnt := (others => '0');
          v.gthErrCnt := (others => '0');
      end if;

      if rxResetDone(i)='0' then
        v := REG_INIT_C;
      end if;
      
      rin(i) <= v;
    end process comb;

    seq : process ( rxUsrClk ) is
    begin
      if rising_edge(rxUsrClk(i)) then
        r(i) <= rin(i);
      end if;
    end process seq;
    
  end generate GEN_CTRL;

  GEN_ILA : if DEBUG_G generate
    U_ILA : ila_0
      port map (
        clk     => stableClk,
        probe0(  7 downto   0) => txbypassrst,
        probe0( 15 downto   8) => txbypassdone,
        probe0( 23 downto  16) => txbypasserr,
        probe0( 31 downto  24) => txpllreset,
        probe0( 39 downto  32) => txUsrRst,
        probe0( 47 downto  40) => txReady,
        probe0( 55 downto  48) => txpmarstdone,
        
        probe0( 63 downto  56) => rxbypassrst,
        probe0( 71 downto  64) => rxbypassdone,
        probe0( 79 downto  72) => rxpllreset,
        probe0( 87 downto  80) => rxReset,
        probe0( 95 downto  88) => rxResetDone,
        probe0(103 downto  96) => rxpmarst,
        probe0(111 downto 104) => rxpmarstdone,
        probe0(119 downto 112) => rxcdrlock,
        probe0(127 downto 120) => rxErrIn,

        probe0(255 downto 128) => (others=>'0') );
  end generate GEN_ILA;
  
end rtl;

