package RISCV.implementation.generic

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic._
import RISCV.model.STALL_REASON

/**
 * <p><b>GenericCore</b> is an abstract class representing a Core module.</p>
 * <p>It provides the basic structure and interface for implementing a processor core.</p>
 * 
 * <h4>IO</h4>
 * <ul>
 *   <li>[[interfaces.core_interface.GeneralInterface]]: Interface providing general signals.</li>
 *   <li>[[interfaces.core_interface.InstructionInterface]]: Interface providing signals related to instruction handling.</li>
 *   <li>[[interfaces.core_interface.DataInterface]]: Interface providing signals related to data access.</li>
 *   <li>[[interfaces.core_interface.RVFIInterface]]: Interface providing signals for the RISC-V formal interface (RVFI).</li>
 * </ul>
 * 
 * @param genPC A function that generates an instance of the ProgramCounter module.
 * @param genRF A function that generates an instance of the RegisterFile module.
 * @param genCU A function that generates an instance of the ControlUnit module.
 * @param genDecoder A function that generates an instance of the Decoder module.
 * @param genBU A function that generates an instance of the BranchUnit module.
 * @param genALU A function that generates an instance of the ALU module.
 */
abstract class GenericCore (genExecutionUnits : Seq[() => AbstractExecutionUnit], genProgramCounter : => AbstractProgramCounter, genRegisterFile : => AbstractRegisterFile) extends Module {

  val io_reset = IO(new ResetInterface)
  val io_instr = IO(new InstructionInterface)
  val io_data = IO(new DataInterface)
  val io_rvfi = IO(new RVFIInterface)

  val executionUnits = genExecutionUnits.map( gen => Module(gen())) 

  val pc = Module(genProgramCounter)
  pc.io_reset.rst_n := io_reset.rst_n
  pc.io_reset.boot_addr := io_reset.boot_addr

  pc.io.pc_we := false.B
  pc.io.pc_wdata := io_reset.boot_addr

  val register_file = Module(genRegisterFile)
  register_file.io_reset.rst_n := io_reset.rst_n
  register_file.io_reset.boot_addr := io_reset.boot_addr
  
  register_file.io.reg_rs1 := 0.U
  register_file.io.reg_rs2 := 0.U
  register_file.io.reg_rd := 0.U
  register_file.io.reg_write_en := false.B
  register_file.io.reg_write_data := 0.U

  io_data.data_addr := 0.U
  io_data.data_req := false.B
  io_data.data_we := false.B
  io_data.data_be := 0.U
  io_data.data_wdata := 0.U

  val misa = Wire(UInt(32.W))
  misa := executionUnits.map(_.io.misa).reduce(_ | _)

  val stall = Wire(STALL_REASON())
  when (io_instr.instr_gnt) {
    stall := STALL_REASON.NO_STALL
  } .otherwise {
    stall := STALL_REASON.INSTR_REQ
  }
  val was_stalled = RegNext(stall)

  val next_instruction = Wire(UInt(32.W))
  val instruction = RegNext(next_instruction)
  next_instruction := io_instr.instr_rdata
  when (~io_reset.rst_n) {
    instruction := Fill(32, 1.U)
  } .elsewhen (stall === STALL_REASON.NO_STALL || stall === STALL_REASON.INSTR_REQ) {
    when (io_instr.instr_gnt) {
        next_instruction := io_instr.instr_rdata
      } .otherwise {
        next_instruction := Fill(32, 1.U)
      }
  } .elsewhen (stall === STALL_REASON.EXECUTION_UNIT) {
    next_instruction := instruction
  }

  val data_gnt = RegNext(io_data.data_gnt)
  val data_rdata = RegNext(io_data.data_rdata)

  io_instr.instr_req := pc.io.pc_we || stall === STALL_REASON.INSTR_REQ
  io_instr.instr_addr := pc.io.pc_wdata

  executionUnits.foreach( unit => {
    unit.io.instr := instruction

    unit.io_pc.pc := pc.io.pc

    unit.io_reg.reg_read_data1 := register_file.io.reg_read_data1
    unit.io_reg.reg_read_data2 := register_file.io.reg_read_data2

    unit.io_data.data_gnt := data_gnt
    unit.io_data.data_rdata := data_rdata

    unit.io_reset.rst_n := io_reset.rst_n
    unit.io_reset.boot_addr := io_reset.boot_addr

    when (unit.io.valid) {
      stall := unit.io.stall

      pc.io.pc_we := unit.io_pc.pc_we
      pc.io.pc_wdata := unit.io_pc.pc_wdata

      register_file.io.reg_rs1 := unit.io_reg.reg_rs1
      register_file.io.reg_rs2 := unit.io_reg.reg_rs2
      register_file.io.reg_rd := unit.io_reg.reg_rd
      register_file.io.reg_write_en := unit.io_reg.reg_write_en
      register_file.io.reg_write_data := unit.io_reg.reg_write_data

      io_data.data_req := unit.io_data.data_req
      io_data.data_addr := unit.io_data.data_addr
      io_data.data_be := unit.io_data.data_be
      io_data.data_we := unit.io_data.data_we
      io_data.data_wdata := unit.io_data.data_wdata

    }
  })

  io_rvfi.rvfi_valid := stall === STALL_REASON.NO_STALL
  
  val order = RegInit(0.U(64.W))
  when (~io_reset.rst_n) {
    order := 0.U
  } .elsewhen (stall === STALL_REASON.NO_STALL) {
    order := order + 1.U
  }
  io_rvfi.rvfi_order := order - 1.U
  io_rvfi.rvfi_insn := instruction
  io_rvfi.rvfi_trap := false.B
  io_rvfi.rvfi_halt := false.B
  io_rvfi.rvfi_intr := false.B
  io_rvfi.rvfi_mode := 0.U
  io_rvfi.rvfi_ixl := 0.U

  io_rvfi.rvfi_rs1_addr := register_file.io.reg_rs1
  io_rvfi.rvfi_rs2_addr := register_file.io.reg_rs2
  io_rvfi.rvfi_rs1_rdata := register_file.io.reg_read_data1
  io_rvfi.rvfi_rs2_rdata := register_file.io.reg_read_data2

  io_rvfi.rvfi_rd_addr := register_file.io.reg_rd
  io_rvfi.rvfi_rd_wdata := Mux(register_file.io.reg_write_en & register_file.io.reg_rd =/= 0.U, register_file.io.reg_write_data, 0.U)

  io_rvfi.rvfi_pc_rdata := pc.io.pc
  io_rvfi.rvfi_pc_wdata := pc.io.pc_wdata

  val had_mem_req = RegInit(false.B)
  when (~io_reset.rst_n) {
    had_mem_req := false.B
  } .elsewhen (io_data.data_req) {
    had_mem_req := true.B
  } .elsewhen(io_rvfi.rvfi_valid) {
    had_mem_req := false.B
  }

  val mem_addr = RegInit(0.U(32.W))
  when (~io_reset.rst_n) {
    mem_addr := 0.U
  } .elsewhen (io_data.data_req) {
    mem_addr := io_data.data_addr
  }

  val mem_rmask = RegInit(0.U(4.W))
  val mem_rdata = RegInit(0.U(32.W))
  val mem_wmask = RegInit(0.U(4.W))
  val mem_wdata = RegInit(0.U(32.W))
  when (~io_reset.rst_n) {
    mem_rmask := 0.U
    mem_rdata := 0.U
    mem_wmask := 0.U
    mem_wdata := 0.U
  } .elsewhen (io_data.data_req & io_data.data_we) {
    mem_rmask := 0.U
    mem_wmask := io_data.data_be
    mem_wdata := io_data.data_wdata & (Fill(8, io_data.data_be(3)) ## Fill(8, io_data.data_be(2)) ## Fill(8, io_data.data_be(1)) ## Fill(8, io_data.data_be(0)))
  } .elsewhen(io_data.data_req) {
    mem_rmask := io_data.data_be
    mem_wmask := 0.U
    mem_wdata := 0.U
  }

  when (io_data.data_gnt & ~io_data.data_we) {
    mem_rdata := io_data.data_rdata & (Fill(8, io_data.data_be(3)) ## Fill(8, io_data.data_be(2)) ## Fill(8, io_data.data_be(1)) ## Fill(8, io_data.data_be(0)))
  } .elsewhen(io_rvfi.rvfi_valid) {
    mem_rdata := 0.U
  }

  when (had_mem_req) {
    io_rvfi.rvfi_mem_addr := mem_addr
    io_rvfi.rvfi_mem_rmask := mem_rmask
    io_rvfi.rvfi_mem_wmask := mem_wmask
    io_rvfi.rvfi_mem_rdata := mem_rdata
    io_rvfi.rvfi_mem_wdata := mem_wdata
  } .otherwise {
    io_rvfi.rvfi_mem_addr := 0.U
    io_rvfi.rvfi_mem_rmask := 0.U
    io_rvfi.rvfi_mem_wmask := 0.U
    io_rvfi.rvfi_mem_rdata := 0.U
    io_rvfi.rvfi_mem_wdata := 0.U
  }
}