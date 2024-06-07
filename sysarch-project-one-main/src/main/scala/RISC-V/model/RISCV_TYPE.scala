package RISCV.model

import chisel3._
import chisel3.util._

import RISCV.utils.Types.ORTYPE

object RISCV_OP extends ChiselEnum {
  val LOAD = Value("b0000011".U)
  val MISC_MEM = Value("b0001111".U)
  val OP_IMM = Value("b0010011".U)
  val AUIPC = Value("b0010111".U)
  val STORE = Value("b0100011".U)
  val OP = Value("b0110011".U)
  val LUI = Value("b0110111".U)
  val BRANCH = Value("b1100011".U)
  val JALR = Value("b1100111".U)
  val JAL = Value("b1101111".U)
  val SYSTEM = Value("b1110011".U)
  val UNKNOWN = Value("b1111111".U)

  def apply(i: UInt): RISCV_OP.Type = {
    return Mux(RISCV_OP.safe(i)._2, RISCV_OP.safe(i)._1, RISCV_OP.UNKNOWN)
  }
}

object RISCV_FUNCT3 extends ChiselEnum {
  val F000 = Value("b000".U)
  val F001 = Value("b001".U)
  val F010 = Value("b010".U)
  val F011 = Value("b011".U)
  val F100 = Value("b100".U)
  val F101 = Value("b101".U)
  val F110 = Value("b110".U)
  val F111 = Value("b111".U)

  def apply(i: UInt): RISCV_FUNCT3.Type = {
    return Mux(RISCV_FUNCT3.safe(i)._2, RISCV_FUNCT3.safe(i)._1, RISCV_FUNCT3.F111)
  }
}

object RISCV_FUNCT7 extends ChiselEnum {
  val ZERO = Value("b0000000".U)
  val MULDIV = Value("b0000001".U)
  val SUB_SRA = Value("b0100000".U)
  val UNKNOWN = Value("b1111111".U)

  def apply(i: UInt): RISCV_FUNCT7.Type = {
    return Mux(RISCV_FUNCT7.safe(i)._2, RISCV_FUNCT7.safe(i)._1, RISCV_FUNCT7.UNKNOWN)
  }
}

object RISCV_FUNCT12 extends ChiselEnum {
  val ECALL = Value("b000000000000".U)
  val EBREAK = Value("b000000000001".U)
  val PAUSE = Value("b000000010000".U)
  val SRET = Value("b000100000010".U)
  val WFI = Value("b000100000101".U)
  val MRET = Value("b001100000010".U)
  val FENCE_ISO = Value("b100000110011".U)
  val UNKNOWN = Value("b111111111111".U)

  def apply(i: UInt): RISCV_FUNCT12.Type = {
    return Mux(RISCV_FUNCT12.safe(i)._2, RISCV_FUNCT12.safe(i)._1, RISCV_FUNCT12.UNKNOWN)
  }
}

object RISCV_TYPE extends ChiselEnum {
  import scala.language.implicitConversions

  implicit private def enumTypeToBigInt[T <: EnumType](value: T): BigInt =
    value.litValue

  implicit private def bigIntToRiscVType[T <: BigInt](
      value: T
  ): RISCV_TYPE.Type = Value(value.U)

  // | op (7) | funct3 (3) | funct7 (7) | funct12 (12) |
  def concat(op: RISCV_OP.Type): RISCV_TYPE.Type = op << 22

  def concat(funct3: RISCV_FUNCT3.Type, op: RISCV_OP.Type): RISCV_TYPE.Type =
    (op << 22) + (funct3 << 19)

  def concat[FUNCT7OR12: ORTYPE[RISCV_FUNCT7.Type, RISCV_FUNCT12.Type]#check](
      funct7or12: FUNCT7OR12,
      funct3: RISCV_FUNCT3.Type,
      op: RISCV_OP.Type
  ): RISCV_TYPE.Type = funct7or12 match {
    case funct7: RISCV_FUNCT7.Type =>
      (op << 22) + (funct3 << 19) + (funct7 << 12)
    case funct12: RISCV_FUNCT12.Type => (op << 22) + (funct3 << 19) + funct12
  }

  def getOP(t: RISCV_TYPE.Type): RISCV_OP.Type = {
    return RISCV_OP(t.asUInt(28,22))
  }

  def getFunct3(t: RISCV_TYPE.Type): RISCV_FUNCT3.Type = {
    return RISCV_FUNCT3(t.asUInt(21, 19))
  }

  def getFunct7(t: RISCV_TYPE.Type): RISCV_FUNCT7.Type = {
    return RISCV_FUNCT7(t.asUInt(18, 12))
  }

  def getFunct12(t: RISCV_TYPE.Type): RISCV_FUNCT12.Type = {
    return RISCV_FUNCT12(t.asUInt(18, 7))
  }

  def apply(i: UInt): RISCV_TYPE.Type = {
    val j = Mux((i.widthKnown & i.widthOption.get < 29).B, i(16,0) ## Fill(12, 0.U), i)
    return Mux(RISCV_TYPE.safe(j)._2, RISCV_TYPE.safe(j)._1, RISCV_TYPE.UNKNOWN)
  }

  val lb = concat(RISCV_FUNCT3.F000, RISCV_OP.LOAD)
  val lh = concat(RISCV_FUNCT3.F001, RISCV_OP.LOAD)
  val lw = concat(RISCV_FUNCT3.F010, RISCV_OP.LOAD)
  val lbu = concat(RISCV_FUNCT3.F100, RISCV_OP.LOAD)
  val lhu = concat(RISCV_FUNCT3.F101, RISCV_OP.LOAD)

  val fence = concat(RISCV_FUNCT3.F000, RISCV_OP.MISC_MEM)
  val pause = concat(RISCV_FUNCT12.PAUSE, RISCV_FUNCT3.F000, RISCV_OP.MISC_MEM)
  val fence_iso = concat(RISCV_FUNCT12.FENCE_ISO, RISCV_FUNCT3.F000, RISCV_OP.MISC_MEM)

  val addi = concat(RISCV_FUNCT3.F000, RISCV_OP.OP_IMM)
  val slli = concat(RISCV_FUNCT7.ZERO, RISCV_FUNCT3.F001, RISCV_OP.OP_IMM)
  val slti = concat(RISCV_FUNCT3.F010, RISCV_OP.OP_IMM)
  val sltiu = concat(RISCV_FUNCT3.F011, RISCV_OP.OP_IMM)
  val xori = concat(RISCV_FUNCT3.F100, RISCV_OP.OP_IMM)
  val srli = concat(RISCV_FUNCT7.ZERO, RISCV_FUNCT3.F101, RISCV_OP.OP_IMM)
  val srai = concat(RISCV_FUNCT7.SUB_SRA, RISCV_FUNCT3.F101, RISCV_OP.OP_IMM)
  val ori = concat(RISCV_FUNCT3.F110, RISCV_OP.OP_IMM)
  val andi = concat(RISCV_FUNCT3.F111, RISCV_OP.OP_IMM)

  val auipc = concat(RISCV_OP.AUIPC)

  val sb = concat(RISCV_FUNCT3.F000, RISCV_OP.STORE)
  val sh = concat(RISCV_FUNCT3.F001, RISCV_OP.STORE)
  val sw = concat(RISCV_FUNCT3.F010, RISCV_OP.STORE)

  val add = concat(RISCV_FUNCT7.ZERO, RISCV_FUNCT3.F000, RISCV_OP.OP)
  val sub = concat(RISCV_FUNCT7.SUB_SRA, RISCV_FUNCT3.F000, RISCV_OP.OP)
  val sll = concat(RISCV_FUNCT7.ZERO, RISCV_FUNCT3.F001, RISCV_OP.OP)
  val slt = concat(RISCV_FUNCT7.ZERO, RISCV_FUNCT3.F010, RISCV_OP.OP)
  val sltu = concat(RISCV_FUNCT7.ZERO, RISCV_FUNCT3.F011, RISCV_OP.OP)
  val xor = concat(RISCV_FUNCT7.ZERO, RISCV_FUNCT3.F100, RISCV_OP.OP)
  val srl = concat(RISCV_FUNCT7.ZERO, RISCV_FUNCT3.F101, RISCV_OP.OP)
  val sra = concat(RISCV_FUNCT7.SUB_SRA, RISCV_FUNCT3.F101, RISCV_OP.OP)
  val or = concat(RISCV_FUNCT7.ZERO, RISCV_FUNCT3.F110, RISCV_OP.OP)
  val and = concat(RISCV_FUNCT7.ZERO, RISCV_FUNCT3.F111, RISCV_OP.OP)

  val lui = concat(RISCV_OP.LUI)

  val beq = concat(RISCV_FUNCT3.F000, RISCV_OP.BRANCH)
  val bne = concat(RISCV_FUNCT3.F001, RISCV_OP.BRANCH)
  val blt = concat(RISCV_FUNCT3.F100, RISCV_OP.BRANCH)
  val bge = concat(RISCV_FUNCT3.F101, RISCV_OP.BRANCH)
  val bltu = concat(RISCV_FUNCT3.F110, RISCV_OP.BRANCH)
  val bgeu = concat(RISCV_FUNCT3.F111, RISCV_OP.BRANCH)

  val jalr = concat(RISCV_OP.JALR)
  val jal = concat(RISCV_OP.JAL)

  val ecall = concat(RISCV_FUNCT12.ECALL, RISCV_FUNCT3.F000, RISCV_OP.SYSTEM)
  val ebreak = concat(RISCV_FUNCT12.EBREAK, RISCV_FUNCT3.F000, RISCV_OP.SYSTEM)

  val sret = concat(RISCV_FUNCT12.SRET, RISCV_FUNCT3.F000, RISCV_OP.SYSTEM)
  val wfi = concat(RISCV_FUNCT12.WFI, RISCV_FUNCT3.F000, RISCV_OP.SYSTEM)
  val mret = concat(RISCV_FUNCT12.MRET, RISCV_FUNCT3.F000, RISCV_OP.SYSTEM)

  val csrrw = concat(RISCV_FUNCT3.F001, RISCV_OP.SYSTEM)
  val csrrs = concat(RISCV_FUNCT3.F010, RISCV_OP.SYSTEM)
  val csrrc = concat(RISCV_FUNCT3.F011, RISCV_OP.SYSTEM)
  val csrrwi = concat(RISCV_FUNCT3.F101, RISCV_OP.SYSTEM)
  val csrrsi = concat(RISCV_FUNCT3.F110, RISCV_OP.SYSTEM)
  val csrrci = concat(RISCV_FUNCT3.F111, RISCV_OP.SYSTEM)

  val UNKNOWN =
    concat(RISCV_FUNCT12.UNKNOWN, RISCV_FUNCT3.F111, RISCV_OP.UNKNOWN)
}
