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

entity XpmSeqJumpReg is
   generic (
      TPD_G            : time            := 1 ns;
      USE_WSTRB_G      : boolean         := false;
      ADDR_BITS_G      : natural         := 12;
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
      config         : out XpmSeqConfigArray(NUM_SEQ_G-1 downto 0);
      -- Clock and Reset
      axiClk         : in  sl;
      axiRst         : in  sl);
end XpmSeqJumpReg;

architecture rtl of XpmSeqJumpReg is

   type RegType is record
      config        : XpmSeqConfigArray(NUM_SEQ_G-1 downto 0);
      axiReadSlave  : AxiLiteReadSlaveType;
      axiWriteSlave : AxiLiteWriteSlaveType;
   end record RegType;

   constant REG_INIT_C : RegType := (
      config        => (others => XPM_SEQ_CONFIG_INIT_C),
      axiReadSlave  => AXI_LITE_READ_SLAVE_INIT_C,
      axiWriteSlave => AXI_LITE_WRITE_SLAVE_INIT_C);

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

begin

   -------------------------------
   -- Configuration Register
   -------------------------------
   comb : process (axiReadMaster, axiRst, axiWriteMaster, r, status) is
      variable v            : RegType;
      variable axiStatus    : AxiLiteStatusType;
      variable axiWriteResp : slv(1 downto 0);
      variable axiReadResp  : slv(1 downto 0);
      variable rdPntr       : natural;
      variable wrPntr       : natural;
      variable iseq         : natural;
      variable ichn         : natural;
      variable regWrData    : slv(31 downto 0);
      variable tmpRdData    : slv(31 downto 0);
      variable regAddr      : slv(31 downto 2);
   begin
      -- Latch the current value
      v := r;

      -- Calculate the address pointers
      wrPntr := conv_integer(axiWriteMaster.awaddr(ADDR_BITS_G-1 downto 2));
      rdPntr := conv_integer(axiReadMaster .araddr(ADDR_BITS_G-1 downto 2));

      -- Determine the transaction type

      -----------------------------
      -- AXI-Lite Write Logic
      -----------------------------
      axiSlaveWaitWriteTxn(axiWriteMaster, v.axiWriteSlave, axiStatus.writeEnable);

      if (axiStatus.writeEnable = '1') then
         -- Check for alignment
         if axiWriteMaster.awaddr(1 downto 0) = "00" then
            -- Address is aligned
            regAddr      := axiWriteMaster.awaddr(regAddr'range);
            regWrData    := axiWriteMaster.wdata;
            axiWriteResp := AXI_RESP_OK_C;
            case wrPntr is
               when 0 to NUM_SEQ_G*16-1 =>
                  iseq                             := conv_integer(regAddr(ADDR_BITS_G-1 downto 6));
                  ichn                             := conv_integer(regAddr(5 downto 2));
                  if (ichn = 15) then
                     v.config(iseq).seqJumpConfig.syncSel   := regWrData(31 downto 16);
                     v.config(iseq).seqJumpConfig.syncClass := regWrData(15 downto 12);
                     v.config(iseq).seqJumpConfig.syncJump  := SeqAddrType(regWrData(SeqAddrType'range));
                  end if;
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
            -- Address is aligned
            tmpRdData   := (others=>'0');
            axiReadResp := AXI_RESP_OK_C;
            -- Decode the read address
            case rdPntr is
               when 0 to NUM_SEQ_G*16-1 =>
                  iseq                             := conv_integer(regAddr(ADDR_BITS_G-1 downto 6));
                  ichn                             := conv_integer(regAddr(5 downto 2));
                  if (ichn = 15) then
                     tmpRdData(31 downto 16)      := r.config(iseq).seqJumpConfig.syncSel;
                     tmpRdData(15 downto 12)      := r.config(iseq).seqJumpConfig.syncClass;
                     tmpRdData(SeqAddrType'range) := slv(r.config(iseq).seqJumpConfig.syncJump);
                  end if;
               when others => tmpRdData := x"DEAD" & regAddr(15 downto 2) & "00";
            end case;
            v.axiReadSlave.rdata := tmpRdData;
            -- Send AXI response
            axiSlaveReadResponse(v.axiReadSlave, axiReadResp);
         else
            axiSlaveReadResponse(v.axiReadSlave, AXI_ERROR_RESP_G);
         end if;
      end if;

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
