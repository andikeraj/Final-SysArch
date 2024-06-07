package RISCV.model

import chisel3._

object REG_WRITE_SEL extends ChiselEnum {
  val ALU_OUT                 = Value("b000".U)
  val MEM_OUT_ZERO_EXTENDED   = Value("b001".U)
  val MEM_OUT_SIGN_EXTENDED   = Value("b010".U)
  val IMM                     = Value("b100".U)
  val PC_PLUS_4               = Value("b101".U)
}

object ALU_OP_1_SEL extends ChiselEnum {
  val RS1 = Value("b0".U)
  val PC = Value("b1".U)
}

object ALU_OP_2_SEL extends ChiselEnum {
  val RS2 = Value("b0".U)
  val IMM = Value("b1".U)
}

object NEXT_PC_SELECT extends ChiselEnum {
  val PC_PLUS_4 = Value("b0".U)
  val ALU_OUT_ALIGNED = Value("b1".U)
  val IMM = Value("b10".U)
  val BRANCH = Value("b11".U)
}

object STALL_REASON extends ChiselEnum {
  val NO_STALL = Value("b00".U)
  val INSTR_REQ = Value("b01".U)
  val EXECUTION_UNIT = Value("b10".U)
}

object ALU_CONTROL extends ChiselEnum{
  val ADD = Value("b0000".U)
  val SLL = Value("b0001".U)
  val SLT = Value("b0010".U)
  val SLTU = Value("b0011".U)
  val XOR = Value("b0100".U)
  val SRL = Value("b0101".U)
  val OR = Value("b0110".U)
  val AND = Value("b0111".U)
  
  val SUB = Value("b1000".U)
  val SRA = Value("b1101".U)

  val UNKNOWN = Value("b1111".U)
  def apply(i: UInt): ALU_CONTROL.Type = {
    return Mux(ALU_CONTROL.safe(i)._2, ALU_CONTROL.safe(i)._1, ALU_CONTROL.UNKNOWN)
  }
}
