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

entity XpmSeqMemReg is
   generic (
      TPD_G            : time            := 1 ns;
      USE_WSTRB_G      : boolean         := false;
      ADDR_BITS_G      : natural         := 14;
      NUM_SEQ_G        : integer         := 1;
      AXI_ERROR_RESP_G : slv(1 downto 0) := AXI_RESP_OK_C);
   port (
      -- AXI-Lite Interface
      axiReadMaster  : in  AxiLiteReadMasterType;
      axiReadSlave   : out AxiLiteReadSlaveType;
      axiWriteMaster : in  AxiLiteWriteMasterType;
      axiWriteSlave  : out AxiLiteWriteSlaveType;
      -- EVR Interface
      status         : in  XpmSeqStatusArray(NUM_SEQ_G-1 downto 0);
      gconfig        : out XpmSeqGConfigType;
      config         : out XpmSeqConfigArray(NUM_SEQ_G-1 downto 0);
      -- Clock and Reset
      axiClk         : in  sl;
      axiRst         : in  sl);
end XpmSeqMemReg;

architecture rtl of XpmSeqMemReg is

   type RegType is record
      gconfig       : XpmSeqGConfigType;
      config        : XpmSeqConfigArray(NUM_SEQ_G-1 downto 0);
      seqRd         : sl;
      seqRdSeq      : slv(3 downto 0);
      seqState      : SequencerState;
      axiReadSlave  : AxiLiteReadSlaveType;
      axiWriteSlave : AxiLiteWriteSlaveType;
      axiRdEn       : slv(2 downto 0);
   end record RegType;

   constant REG_INIT_C : RegType := (
      gconfig       => XPM_SEQ_GCONFIG_INIT_C,
      config        => (others=>XPM_SEQ_CONFIG_INIT_C),
      seqRd         => '0',
      seqRdSeq      => (others => '0'),
      seqState      => SEQUENCER_STATE_INIT_C,
      axiReadSlave  => AXI_LITE_READ_SLAVE_INIT_C,
      axiWriteSlave => AXI_LITE_WRITE_SLAVE_INIT_C,
      axiRdEn       => (others => '0'));

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

begin

   -------------------------------
   -- Configuration Register
   -------------------------------
   comb : process (axiReadMaster, axiWriteMaster, r, status) is
      variable v            : RegType;
      variable axiStatus    : AxiLiteStatusType;
      variable axiWriteResp : slv(1 downto 0);
      variable axiReadResp  : slv(1 downto 0);
      variable rdPntr       : natural;
      variable wrPntr       : natural;
      variable iseq         : natural;
      variable ichn         : natural;
      variable regWrData    : slv(31 downto 0);
      variable regAddr      : slv(31 downto 2);
   begin
      -- Latch the current value
      v := r;

      -- Calculate the address pointers
      wrPntr := conv_integer(axiWriteMaster.awaddr(ADDR_BITS_G-1 downto 2));
      rdPntr := conv_integer(axiReadMaster .araddr(ADDR_BITS_G-1 downto 2));

      -- Reset strobing signals
      for i in 0 to NUM_SEQ_G-1 loop
        v.config(i).seqWrEn := '0';
      end loop;
      v.seqRd          := '0';
      v.seqRdSeq       := (others => '0');
      v.axiRdEn        := r.axiRdEn(1 downto 0) & '0';

      -----------------------------
      -- AXI-Lite Write Logic
      -----------------------------

      axiSlaveWaitWriteTxn(axiWriteMaster, v.axiWriteSlave, axiStatus.writeEnable);

      if (axiStatus.writeEnable = '1') then
         -- Check for alignment
         if axiWriteMaster.awaddr(1 downto 0) = "00" then
            -- Update external data/address buses
            v.gconfig.seqWrData := axiWriteMaster.wdata;
            -- Address is aligned
            regAddr            := axiWriteMaster.awaddr(regAddr'range);
            regWrData          := axiWriteMaster.wdata;
            axiWriteResp       := AXI_RESP_OK_C;
            case wrPntr is
               when 0 to NUM_SEQ_G*2048-1 =>
                  iseq                   := conv_integer(regAddr(ADDR_BITS_G-1 downto SEQADDRLEN+2));
                  v.config(iseq).seqWrEn := '1';
                  v.gconfig.seqAddr      := SeqAddrType(regAddr(SEQADDRLEN+1 downto 2));
               when others => axiWriteResp := AXI_ERROR_RESP_G;
            end case;
            axiSlaveWriteResponse(v.axiWriteSlave, axiWriteResp);
         else                           -- if axiWriteMaster.awaddr(1 downto 0) = "00"
            axiSlaveWriteResponse(v.axiWriteSlave, AXI_ERROR_RESP_G);
         end if;
      end if;

      -----------------------------
      -- AXI-Lite Read Logic
      -----------------------------

      axiSlaveWaitReadTxn(axiReadMaster, v.axiReadSlave, axiStatus.readEnable);

      if (axiStatus.readEnable = '1') then
         -- Reset the bus
         regAddr              := axiReadMaster.araddr(regAddr'range);
         -- Check for alignment
         if axiReadMaster.araddr(1 downto 0) = "00" then
            -- Update external data/address buses
            v.gconfig.seqAddr := SeqAddrType(regAddr(SEQADDRLEN+1 downto 2));
            -- Address is aligned
            axiReadResp      := AXI_RESP_OK_C;
            -- BRAM 2 cycles read delay
            v.axiRdEn        := r.axiRdEn(1 downto 0) & '1';
            -- Check if BRAM is valid
            if r.axiRdEn(1) = '1' then
              v.seqRdSeq := resize(regAddr(ADDR_BITS_G-1 downto SEQADDRLEN+2),
                                   r.seqRdSeq'length);
            end if;
            if r.axiRdEn(2) = '1' then
               -- Decode the read address
               case rdPntr is
                  when 0 to NUM_SEQ_G*2048-1 =>
                     iseq       := conv_integer(regAddr(ADDR_BITS_G-1 downto SEQADDRLEN+2));
                     v.seqRd    := '1';
                     v.seqRdSeq := std_logic_vector(conv_unsigned(iseq, v.seqRdSeq'length));
                  when others => v.axiReadSlave.rdata := x"DEAD" & regAddr(15 downto 2) & "00";
               end case;
               -- Send AXI response
               axiSlaveReadResponse(v.axiReadSlave, axiReadResp);
            end if;
         else
            axiSlaveReadResponse(v.axiReadSlave, AXI_ERROR_RESP_G);
         end if;
      end if;

      if r.seqRd = '1' then
         v.axiReadSlave.rdata := status(conv_integer(r.seqRdSeq)).seqRdData;
      end if;

      -- Register the variable for next clock cycle
      rin <= v;

      -- Outputs
      axiWriteSlave <= r.axiWriteSlave;
      axiReadSlave  <= r.axiReadSlave;

      config          <= r.config;
      gconfig.seqAddr <= v.gconfig.seqAddr;
   end process comb;

   seq : process (axiClk) is
   begin
      if rising_edge(axiClk) then
         r <= rin after TPD_G;
      end if;
   end process seq;

end rtl;
