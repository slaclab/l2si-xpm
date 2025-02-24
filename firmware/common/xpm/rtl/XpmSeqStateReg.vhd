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

entity XpmSeqStateReg is
   generic (
      TPD_G            : time            := 1 ns;
      USE_WSTRB_G      : boolean         := false;
      NUM_SEQ_G        : integer         := 1;
      AXI_ERROR_RESP_G : slv(1 downto 0) := AXI_RESP_OK_C);
   port (
      -- AXI-Lite Interface
      axiReadMaster  : in  AxiLiteReadMasterType;
      axiReadSlave   : out AxiLiteReadSlaveType;
      axiWriteMaster : in  AxiLiteWriteMasterType;
      axiWriteSlave  : out AxiLiteWriteSlaveType;
      seqRestart     : in  slv(NUM_SEQ_G-1 downto 0);
      seqDisable     : in  slv(NUM_SEQ_G-1 downto 0);
      -- EVR Interface
      status         : in  XpmSeqStatusArray(NUM_SEQ_G-1 downto 0);
      config         : out XpmSeqConfigArray(NUM_SEQ_G-1 downto 0);
      -- Clock and Reset
      axiClk         : in  sl;
      axiRst         : in  sl);
end XpmSeqStateReg;

architecture rtl of XpmSeqStateReg is

   type RegType is record
      config        : XpmSeqConfigArray(NUM_SEQ_G-1 downto 0);
      seqState      : SequencerState;
      axiReadSlave  : AxiLiteReadSlaveType;
      axiWriteSlave : AxiLiteWriteSlaveType;
   end record RegType;

   constant REG_INIT_C : RegType := (
      config        => (others=>XPM_SEQ_CONFIG_INIT_C),
      seqState      => SEQUENCER_STATE_INIT_C,
      axiReadSlave  => AXI_LITE_READ_SLAVE_INIT_C,
      axiWriteSlave => AXI_LITE_WRITE_SLAVE_INIT_C);

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

begin

   -------------------------------
   -- Configuration Register
   -------------------------------
   comb : process (axiReadMaster, axiRst, axiWriteMaster, r, status,
                   seqRestart, seqDisable) is
      variable v  : RegType;
      variable ep : AxiLiteEndpointType;
   begin
      -- Latch the current value
      v                   := r;

      axiSlaveWaitTxn(ep, axiWriteMaster, axiReadMaster, v.axiWriteSlave, v.axiReadSlave);

      axiSlaveRegisterR(ep, toSlv(0, 12), 0, toSlv(SEQADDRLEN, 4));
      axiSlaveRegisterR(ep, toSlv(0, 12), 16, toSlv(MAXEXPSEQDEPTH, 8));
      axiSlaveRegisterR(ep, toSlv(0, 12), 24, toSlv(NUM_SEQ_G, 8));

      for i in 0 to NUM_SEQ_G-1 loop
         v.config(i).seqRestart := '0';
         axiSlaveRegister (ep, toSlv(4, 12), i+0, v.config(i).seqEnable);
         axiSlaveRegister (ep, toSlv(8, 12), i+0, v.config(i).seqRestart);

         if seqRestart(i) = '1' then
            v.config(i).seqRestart := '1';
            v.config(i).seqEnable  := '1';
         elsif seqDisable(i) = '1' then
            v.config(i).seqEnable  := '0';
         end if;
         
         if (axiReadMaster.araddr(6 downto 4) = i) then
            v.seqState := status(i).seqState;
         end if;

         axiSlaveRegisterR(ep, toSlv(i*16+132, 12), 0, status(i).countInvalid);
         axiSlaveRegisterR(ep, toSlv(i*16+136, 12), 0, slv(status(i).seqState.index));
         for j in 0 to 3 loop
            axiSlaveRegisterR(ep, toSlv(i*16+140, 12), j*4, r.seqState.count(j));
         end loop;
      end loop;

      axiSlaveDefault(ep, v.axiWriteSlave, v.axiReadSlave);

      -- Register the variable for next clock cycle
      rin <= v;

      -- Outputs
      axiWriteSlave <= r.axiWriteSlave;
      axiReadSlave  <= r.axiReadSlave;

      config <= r.config;
   end process comb;

   seq : process (axiClk) is
   begin
      if rising_edge(axiClk) then
         r <= rin after TPD_G;
      end if;
   end process seq;

end rtl;
