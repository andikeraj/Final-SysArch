package RISCV.implementation.generic

import chisel3._

import RISCV.interfaces.generic.AbstractProgramCounter

class ProgramCounter extends AbstractProgramCounter{
  val pc = RegInit(0.U(32.W))
  io.pc := pc
  when(~io_reset.rst_n){
    pc := io_reset.boot_addr
  }.elsewhen(io.pc_we){
    pc := io.pc_wdata
  }
}
