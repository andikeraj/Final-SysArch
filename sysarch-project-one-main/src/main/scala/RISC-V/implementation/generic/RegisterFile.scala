package RISCV.implementation.generic

import chisel3._

import RISCV.interfaces.generic.AbstractRegisterFile

class RegisterFile extends AbstractRegisterFile {
  val registers = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  io.reg_read_data1 := Mux(io.reg_rs1 =/= 0.U, registers(io.reg_rs1), 0.U(32.W))
  io.reg_read_data2 := Mux(io.reg_rs2 =/= 0.U, registers(io.reg_rs2), 0.U(32.W))

  when(~io_reset.rst_n) {
    registers := VecInit(Seq.fill(32)(0.U(32.W)))
  } .elsewhen(io.reg_write_en & io.reg_rd =/= 0.U) {
    registers(io.reg_rd) := io.reg_write_data
  }
}