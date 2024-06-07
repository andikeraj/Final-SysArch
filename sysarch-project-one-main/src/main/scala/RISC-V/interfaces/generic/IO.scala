package RISCV.interfaces.generic

import chisel3._

import RISCV.model.STALL_REASON

class ResetInterface extends Bundle {
  val rst_n = Input(Bool())
  val boot_addr = Input(UInt(32.W))
}

class PCInterface extends Bundle {
    val pc = Input(UInt(32.W))
    val pc_we = Output(Bool())
    val pc_wdata = Output(UInt(32.W))
}

class RegisterInterface extends Bundle{
    val reg_rs1 = Output(UInt(5.W))
    val reg_rs2 = Output(UInt(5.W))
    val reg_rd = Output(UInt(5.W))
    val reg_write_en = Output(Bool())
    val reg_write_data = Output(UInt(32.W))
    val reg_read_data1 = Input(UInt(32.W))
    val reg_read_data2 = Input(UInt(32.W))
}

class InstructionInterface extends Bundle {
    // Instruction Interface
    val instr_req = Output(Bool())
    val instr_gnt = Input(Bool())
    val instr_addr = Output(UInt(32.W))
    val instr_rdata = Input(UInt(32.W))
}

class DataInterface extends Bundle {
    // Data Interface
    val data_req = Output(Bool())
    val data_gnt = Input(Bool())
    val data_addr = Output(UInt(32.W))
    val data_be = Output(UInt(4.W))
    val data_we = Output(Bool())
    val data_wdata = Output(UInt(32.W))
    val data_rdata = Input(UInt(32.W))
}

class ExecutionUnitInterface extends Bundle {
  val misa = Output(UInt(32.W))
  val valid = Output(Bool())
  val stall = Output(STALL_REASON())
  val instr = Input(UInt(32.W))
}

class RVFIInterface extends Bundle {
    // RISC-V Formal Interface
    val rvfi_valid = Output(Bool())
    val rvfi_order = Output(UInt(64.W))
    val rvfi_insn = Output(UInt(32.W))
    val rvfi_trap = Output(Bool())
    val rvfi_halt = Output(Bool())
    val rvfi_intr = Output(Bool())
    val rvfi_mode = Output(UInt(2.W))
    val rvfi_ixl = Output(UInt(2.W))

    val rvfi_rs1_addr = Output(UInt(5.W))
    val rvfi_rs2_addr = Output(UInt(5.W))
    val rvfi_rs1_rdata = Output(UInt(32.W))
    val rvfi_rs2_rdata = Output(UInt(32.W))

    val rvfi_rd_addr = Output(UInt(5.W))
    val rvfi_rd_wdata = Output(UInt(32.W))

    val rvfi_pc_rdata = Output(UInt(32.W))
    val rvfi_pc_wdata = Output(UInt(32.W))

    val rvfi_mem_addr = Output(UInt(32.W))
    val rvfi_mem_rmask = Output(UInt(4.W))
    val rvfi_mem_wmask = Output(UInt(4.W))
    val rvfi_mem_rdata = Output(UInt(32.W))
    val rvfi_mem_wdata = Output(UInt(32.W))
}
