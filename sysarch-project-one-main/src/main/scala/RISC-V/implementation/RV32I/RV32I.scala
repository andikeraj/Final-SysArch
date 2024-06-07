package RISCV.implementation.RV32I

import chisel3._
import chisel3.util._

import RISCV.interfaces.RV32I._
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class RV32I (
    genCU : => AbstractControlUnit,
    genDecoder : => AbstractDecoder,
    genBU : => AbstractBranchUnit,
    genALU : => AbstractALU
  ) extends AbstractExecutionUnit {

  io.misa := "b01__0000__0_00000_00000_00000_01000_00000".U

  // Instantiate modules Control Unit, Decoder, Branch Unit, ALU
  val control_unit = Module(genCU)
  control_unit.io_reset <> io_reset

  val decoder = Module(genDecoder)
  decoder.io_reset <> io_reset

  val branch_unit = Module(genBU)
  branch_unit.io_reset <> io_reset

  val alu = Module(genALU)
  alu.io_reset <> io_reset

  val stalled = RegInit(STALL_REASON.NO_STALL)
  when(~io_reset.rst_n) {
    stalled := STALL_REASON.NO_STALL
  } .otherwise {
    stalled := control_unit.io_ctrl.stall
  }

  decoder.io_decoder.instr := io.instr
  io.valid := decoder.io_decoder.valid
  io.stall := control_unit.io_ctrl.stall

  // Assign the program counter interface
  io_pc.pc_wdata := io_reset.boot_addr // Default value
  switch (control_unit.io_ctrl.next_pc_select) {
    is(NEXT_PC_SELECT.PC_PLUS_4) {
      io_pc.pc_wdata := io_pc.pc + 4.U
    }
    is(NEXT_PC_SELECT.BRANCH) {
      io_pc.pc_wdata := Mux(branch_unit.io_branch.branch_taken, io_pc.pc + decoder.io_decoder.imm, (io_pc.pc + 4.U))
    }
  }
  io_pc.pc_we := control_unit.io_ctrl.stall === STALL_REASON.NO_STALL

  // Assign the register file interface
  io_reg.reg_rs1 := decoder.io_decoder.rs1
  io_reg.reg_rs2 := decoder.io_decoder.rs2
  io_reg.reg_rd := decoder.io_decoder.rd
  io_reg.reg_write_en := control_unit.io_ctrl.reg_we
  io_reg.reg_write_data := 0.U
  switch(control_unit.io_ctrl.reg_write_sel) {
    is(REG_WRITE_SEL.ALU_OUT) {
      io_reg.reg_write_data := alu.io_alu.result
    }
    is(REG_WRITE_SEL.IMM) {
      io_reg.reg_write_data := decoder.io_decoder.imm
    }
    is(REG_WRITE_SEL.PC_PLUS_4) {
      io_reg.reg_write_data := io_pc.pc + 4.U
    }
  }
  
  // Assign the control unit inputs
  control_unit.io_ctrl.instr_type := decoder.io_decoder.instr_type
  control_unit.io_ctrl.data_gnt := io_data.data_gnt

  // Assign the ALU inputs
  alu.io_alu.op1 := 0.U
  switch (control_unit.io_ctrl.alu_op_1_sel) {
    is(ALU_OP_1_SEL.RS1) {
      alu.io_alu.op1 := io_reg.reg_read_data1
    }
    is(ALU_OP_1_SEL.PC) {
      alu.io_alu.op1 := io_pc.pc
    }
  }
  alu.io_alu.op2 := 0.U
  switch (control_unit.io_ctrl.alu_op_2_sel) {
    is(ALU_OP_2_SEL.RS2) {
      alu.io_alu.op2 := io_reg.reg_read_data2
    }
    is(ALU_OP_2_SEL.IMM) {
      alu.io_alu.op2 := decoder.io_decoder.imm
    }
  }
  alu.io_alu.alu_op := control_unit.io_ctrl.alu_control

  // Let the branch unit decide if we need to take a branch based on the ALU result and the instruction type
  branch_unit.io_branch.instr_type := decoder.io_decoder.instr_type
  branch_unit.io_branch.alu_out := alu.io_alu.result

  // Assign the data interface
  io_data.data_req := control_unit.io_ctrl.data_req
  io_data.data_addr := alu.io_alu.result            // The address of memory accesses is the result of the ALU
  io_data.data_be := control_unit.io_ctrl.data_be
  io_data.data_we := control_unit.io_ctrl.data_we
  io_data.data_wdata := io_reg.reg_read_data2       // If we write, we are using the content of the second register as data
}
