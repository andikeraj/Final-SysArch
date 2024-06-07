package RISCV.interfaces.RV32I

import chisel3._

import RISCV.interfaces.generic._

abstract class AbstractALU extends Module {
    val io_alu = IO(new ALUInterface)
    val io_reset = IO(new ResetInterface)
}

abstract class AbstractBranchUnit extends Module {
    val io_branch = IO(new BranchUnitInterface)
    val io_reset = IO(new ResetInterface)
  
}

abstract class AbstractControlUnit extends Module {
    val io_ctrl = IO(new ControlUnitInterface)
    val io_reset = IO(new ResetInterface)
}

abstract class AbstractDecoder extends Module{
  val io_decoder = IO(new DecoderInterface)
  val io_reset = IO(new ResetInterface)
}