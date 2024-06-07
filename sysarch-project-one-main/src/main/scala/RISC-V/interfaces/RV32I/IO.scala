package RISCV.interfaces.RV32I

import chisel3._

import RISCV.model._

class ALUInterface extends Bundle {
  val op1 = Input(UInt(32.W))
  val op2 = Input(UInt(32.W))
  val alu_op = Input(ALU_CONTROL())
  val result = Output(UInt(32.W))
}

class BranchUnitInterface extends Bundle {
  val instr_type = Input(RISCV_TYPE())
  val alu_out = Input(UInt(32.W))
  val branch_taken = Output(Bool())
}

class ControlUnitInterface extends Bundle {
  val instr_type = Input(RISCV_TYPE())

  val stall = Output(STALL_REASON())

  val reg_we = Output(Bool())
  val reg_write_sel = Output(REG_WRITE_SEL())

  val alu_control = Output(ALU_CONTROL())
  val alu_op_1_sel = Output(ALU_OP_1_SEL())
  val alu_op_2_sel = Output(ALU_OP_2_SEL())

  val data_req = Output(Bool())
  val data_gnt = Input(Bool())
  val data_we = Output(Bool())
  val data_be = Output(UInt(4.W))

  val next_pc_select = Output(NEXT_PC_SELECT())
}

class DecoderInterface extends Bundle {
  val instr = Input(UInt(32.W))
  val valid = Output(Bool())
  val instr_type = Output(RISCV_TYPE())
  val rs1 = Output(UInt(5.W))
  val rs2 = Output(UInt(5.W))
  val rd = Output(UInt(5.W))
  val imm = Output(UInt(32.W))
}

