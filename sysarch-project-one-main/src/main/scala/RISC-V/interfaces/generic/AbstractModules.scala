package RISCV.interfaces.generic

import chisel3._

abstract class AbstractExecutionUnit extends Module {
  val io = IO(new ExecutionUnitInterface)
  val io_reset = IO(new ResetInterface)
  val io_pc = IO(new PCInterface)
  val io_reg = IO(new RegisterInterface)
  val io_data = IO(new DataInterface)
}

abstract class AbstractProgramCounter extends Module {
  val io = IO(Flipped(new PCInterface))
  val io_reset = IO(new ResetInterface)
}

abstract class AbstractRegisterFile extends Module{
    val io = IO(Flipped(new RegisterInterface))
    val io_reset = IO(new ResetInterface)
}
