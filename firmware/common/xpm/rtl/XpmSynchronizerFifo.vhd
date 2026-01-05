-------------------------------------------------------------------------------
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: Synchronizing FIFO wrapper
-------------------------------------------------------------------------------
-- This file is part of 'SLAC Firmware Standard Library'.
-- It is subject to the license terms in the LICENSE.txt file found in the
-- top-level directory of this distribution and at:
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
-- No part of 'SLAC Firmware Standard Library', including this file,
-- may be copied, modified, propagated, or distributed except according to
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

library lcls_timing_core;
use lcls_timing_core.TimingPkg.all;

library surf;
use surf.StdRtlPkg.all;

entity XpmSynchronizerFifo is
   generic (
      TPD_G          : time                       := 1 ns;
      COMMON_CLK_G   : boolean                    := false); -- Bypass FifoAsync module for synchronous data configuration
   port (
      -- Asynchronous Reset
      rst    : in  sl := '0';
      -- Write Ports (wr_clk domain)
      wr_clk : in  sl;
      din    : in  slv(17 downto 0);
      -- Read Ports (rd_clk domain)
      rd_clk : in  sl;
      valid  : out sl;
      dout   : out slv(17 downto 0));
end XpmSynchronizerFifo;

architecture rtl of XpmSynchronizerFifo is

   constant DATA_WIDTH_C : integer := 18;
   constant ADDR_WIDTH_C : integer := 4;

   constant IDLE_C : slv(17 downto 0) := "01" & D_215_C & K_COM_C;

   signal wr_en, rd_en : sl;
   signal wr_cnt, rd_cnt : slv(ADDR_WIDTH_C-1 downto 0);
   signal com_detected : sl;
   constant LO_MARK_C : slv(ADDR_WIDTH_C-1 downto 0) := toSlv(2,ADDR_WIDTH_C);
   constant HI_MARK_C : slv(ADDR_WIDTH_C-1 downto 0) := toSlv((2**ADDR_WIDTH_C)-3,
                                                              ADDR_WIDTH_C);
   
begin

   GEN_ASYNC : if (COMMON_CLK_G = false) generate

      FifoAsync_1 : entity surf.FifoAsync
         generic map (
            TPD_G          => TPD_G,
            DATA_WIDTH_G   => DATA_WIDTH_C,
            ADDR_WIDTH_G   => ADDR_WIDTH_C)
         port map (
            rst           => rst,
            wr_clk        => wr_clk,
            wr_en         => wr_en,
            din           => din,
            wr_data_count => wr_cnt,
            wr_ack        => open,
            overflow      => open,
            prog_full     => open,
            almost_full   => open,
            full          => open,
            rd_clk        => rd_clk,
            rd_en         => rd_en,
            dout          => dout,
            rd_data_count => rd_cnt,
            valid         => valid,
            underflow     => open,
            prog_empty    => open,
            almost_empty  => open,
            empty         => open);

      -- Drop idle characters if the FIFO is running full
      com_detected <= '1' when (din (16)='1' and din(7 downto 0)=K_COM_C) else '0';
      wr_en <= '0' when (com_detected = '1' and
                         wr_cnt > HI_MARK_C) else '1';
      -- Duplicate idle characters if the FIFO is running empty
      rd_en <= '0' when (com_detected = '1' and
                         rd_cnt < LO_MARK_C) else '1';
   end generate;

   GEN_SYNC : if (COMMON_CLK_G = true) generate
      dout  <= din;
      valid <= wr_en;
   end generate;

end architecture rtl;
