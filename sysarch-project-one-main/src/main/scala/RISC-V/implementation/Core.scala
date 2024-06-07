package RISCV.implementation

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.implementation.generic._

class Core (genExecutionUnits : Seq[() => AbstractExecutionUnit]) extends GenericCore(genExecutionUnits, new ProgramCounter, new RegisterFile) {
  // Nothing to do here.
}
