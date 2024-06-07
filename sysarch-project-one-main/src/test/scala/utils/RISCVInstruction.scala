package utils

import chisel3._
import chisel3.util._

import scala.util.Random
import RISCV.utils.assembler.RISCVAssembler

object MyRandom {
    val rnd = new Random(12345678)
    def nextInt(l: Int): Int = {
        rnd.nextInt(l)
    }
}

final case class RISCVInstruction(instruction: String, assembly: UInt, effect: RVFI, state: ProcessorState = null) {}

object RISCVInstruction {

    def getSignedValue(value: Long, size: Int): Long = {
        // Ensure that size is within a valid range
        require(size > 0 && size <= 64, "Size must be between 1 and 64")

        // Mask the value to the size
        val mask = (1L << size) - 1
        val maskedValue = value & mask
        
        // Check the sign bit (the most significant bit in the size)
        val signBit = 1L << (size - 1)
        
        // If the sign bit is set, extend the sign
        if ((maskedValue & signBit) != 0) {
            maskedValue | ~mask
        } else {
            maskedValue
        }
    }

    def signedToUnsigned(value: Long, size: Int): Long = {
        // Ensure that size is within a valid range
        require(size > 0 && size <= 64, "Size must be between 1 and 64")

        // Mask the value to the size
        val mask = (1L << size) - 1
        val signExtendedValue = value & mask

        // Return the sign-extended unsigned representation
        signExtendedValue
    }

    def getRandomImmediate(bits: Int): Int = {
        val max = 1 << bits
        MyRandom.nextInt(max)
    }

    def getRandomRegister(may_be_zero: Boolean): Int = {
        if (may_be_zero) MyRandom.nextInt(32) else MyRandom.nextInt(31) + 1
    }

    def getRandomMemoryValue(): Long = {
        MyRandom.nextInt(1 << 32)
    }

    def unsignedToSigned(value: Long, size: Int): Long = {
        // Ensure that size is within a valid range
        require(size > 0 && size <= 64, "Size must be between 1 and 64")

        // Determine the sign bit position
        val signBit = 1L << (size - 1)

        // Check if the sign bit is set
        if ((value & signBit) != 0) {
            // Perform sign extension
            val mask = (1L << size) - 1
            value | ~mask
        } else {
            // Return the unsigned value directly
            value
        }
    }

    def getImmediateAsString(imm: Int, width: Int): String = {
        var imm_string = " " + imm.toString()
        if (getSignedValue(imm, width) < 0) {
            imm_string = " -" + (-getSignedValue(imm, width)).toString()
        }
        imm_string
    }


    def getADDI(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "addi x" + rd + " x" + rs1 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong + getSignedValue(imm, imm_width), 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSLTI(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "slti x" + rd + " x" + rs1 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = if (unsignedToSigned(state.registers(rs1).toLong, 32) < getSignedValue(imm, imm_width)) 1.U else 0.U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSLTIU(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "sltiu x" + rd + " x" + rs1 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = if (state.registers(rs1) < signedToUnsigned(getSignedValue(imm, imm_width), 32)) 1.U else 0.U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getANDI(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "andi x" + rd + " x" + rs1 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong & signedToUnsigned(getSignedValue(imm, imm_width), 32), 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getORI(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "ori x" + rd + " x" + rs1 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong | signedToUnsigned(getSignedValue(imm, imm_width), 32), 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getXORI(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "xori x" + rd + " x" + rs1 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong ^ signedToUnsigned(getSignedValue(imm, imm_width), 32), 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSLLI(state: ProcessorState): RISCVInstruction = {
        val imm_width = 5
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "slli x" + rd + " x" + rs1 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong << imm, 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSRLI(state: ProcessorState): RISCVInstruction = {
        val imm_width = 5
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "srli x" + rd + " x" + rs1 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong >>> imm, 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSRAI(state: ProcessorState): RISCVInstruction = {
        val imm_width = 5
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "srai x" + rd + " x" + rs1 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong >> imm, 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getLUI(state: ProcessorState): RISCVInstruction = {
        val imm_width = 20
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val instruction = "lui x" + rd + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = 0.U
            rs2_addr = 0.U
            rs1_rdata = 0.U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(imm << 12, 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getAUIPC(state: ProcessorState): RISCVInstruction = {
        val imm_width = 20
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val instruction = "auipc x" + rd  + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = 0.U
            rs2_addr = 0.U
            rs1_rdata = 0.U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.pc + (imm << 12), 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getADD(state: ProcessorState): RISCVInstruction = {
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "add x" + rd + " x" + rs1 + " x" + rs2
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong + state.registers(rs2).toLong, 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSLT(state: ProcessorState): RISCVInstruction = {
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "slt x" + rd + " x" + rs1 + " x" + rs2
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = rd.U
            rd_wdata = if (unsignedToSigned(state.registers(rs1).toLong, 32) < unsignedToSigned(state.registers(rs2).toLong, 32)) 1.U else 0.U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSLTU(state: ProcessorState): RISCVInstruction = {
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "sltu x" + rd + " x" + rs1 + " x" + rs2
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = rd.U
            rd_wdata = if (state.registers(rs1) < state.registers(rs2)) 1.U else 0.U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getAND(state: ProcessorState): RISCVInstruction = {
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "and x" + rd + " x" + rs1 + " x" + rs2
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong & state.registers(rs2).toLong, 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getOR(state: ProcessorState): RISCVInstruction = {
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "or x" + rd + " x" + rs1 + " x" + rs2
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong | state.registers(rs2).toLong, 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getXOR(state: ProcessorState): RISCVInstruction = {
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "xor x" + rd + " x" + rs1 + " x" + rs2
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong ^ state.registers(rs2).toLong, 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSLL(state: ProcessorState): RISCVInstruction = {
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "sll x" + rd + " x" + rs1 + " x" + rs2
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong << (state.registers(rs2).toInt & 0x1F), 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSRL(state: ProcessorState): RISCVInstruction = {
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "srl x" + rd + " x" + rs1 + " x" + rs2
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong >>> (state.registers(rs2).toInt & 0x1F), 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSUB(state: ProcessorState): RISCVInstruction = {
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "sub x" + rd + " x" + rs1 + " x" + rs2
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong - state.registers(rs2).toLong, 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSRA(state: ProcessorState): RISCVInstruction = {
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "sra x" + rd + " x" + rs1 + " x" + rs2
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(state.registers(rs1).toLong >> (state.registers(rs2).toInt & 0x1F), 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getJAL(state: ProcessorState): RISCVInstruction = {
        val imm_width = 20
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val instruction = "jal x" + rd  + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = 0.U
            rs2_addr = 0.U
            rs1_rdata = 0.U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = (state.pc + 4).U
            pc_rdata = state.pc.U
            pc_wdata = signedToUnsigned((state.pc + getSignedValue(imm, imm_width)) & ~0x1, 32).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getJALR(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "jalr x" + rd + " x" + rs1 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = (state.pc + 4).U
            pc_rdata = state.pc.U
            pc_wdata = signedToUnsigned(unsignedToSigned(state.registers(rs1).toLong, 32) + getSignedValue(imm, imm_width) & ~0x1, 32).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getBEQ(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        var imm_string = "0x" + imm.toHexString
        val instruction = "beq x" + rs1 + " x" + rs2 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = 0.U
            rd_wdata = 0.U
            pc_rdata = state.pc.U
            pc_wdata = if (state.registers(rs1) == state.registers(rs2)) signedToUnsigned(state.pc + getSignedValue(imm, imm_width) & ~0x1, 32).U else (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getBNE(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        var imm_string = "0x" + imm.toHexString
        val instruction = "bne x" + rs1 + " x" + rs2 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = 0.U
            rd_wdata = 0.U
            pc_rdata = state.pc.U
            pc_wdata = if (state.registers(rs1) != state.registers(rs2)) signedToUnsigned(state.pc + getSignedValue(imm, imm_width) & ~0x1, 32).U else (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getBLT(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        var imm_string = "0x" + imm.toHexString
        val instruction = "blt x" + rs1 + " x" + rs2 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = 0.U
            rd_wdata = 0.U
            pc_rdata = state.pc.U
            pc_wdata = if (unsignedToSigned(state.registers(rs1).toLong, 32) < unsignedToSigned(state.registers(rs2).toLong, 32)) signedToUnsigned(state.pc + getSignedValue(imm, imm_width) & ~0x1, 32).U else (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getBLTU(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        var imm_string = "0x" + imm.toHexString
        val instruction = "bltu x" + rs1 + " x" + rs2 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = 0.U
            rd_wdata = 0.U
            pc_rdata = state.pc.U
            pc_wdata = if (state.registers(rs1) < state.registers(rs2)) signedToUnsigned(state.pc + getSignedValue(imm, imm_width) & ~0x1, 32).U else (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getBGE(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        var imm_string = "0x" + imm.toHexString
        val instruction = "bge x" + rs1 + " x" + rs2 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = 0.U
            rd_wdata = 0.U
            pc_rdata = state.pc.U
            pc_wdata = if (unsignedToSigned(state.registers(rs1).toLong, 32) >= unsignedToSigned(state.registers(rs2).toLong, 32)) signedToUnsigned(state.pc + getSignedValue(imm, imm_width) & ~0x1, 32).U else (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getBGEU(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "bgeu x" + rs1 + " x" + rs2 + getImmediateAsString(imm, imm_width)
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = 0.U
            rd_wdata = 0.U
            pc_rdata = state.pc.U
            pc_wdata = if (state.registers(rs1) >= state.registers(rs2)) signedToUnsigned(state.pc + getSignedValue(imm, imm_width) & ~0x1, 32).U else (state.pc + 4).U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getLB(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "lb x" + rd + " " + getImmediateAsString(imm, imm_width) + "(x" + rs1 + ")"
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val address = state.registers(rs1).toLong + getSignedValue(imm, imm_width)
        val value = getRandomMemoryValue()
        val offset = BigInt(signedToUnsigned(address, 32)) % 4
        val mem_value = (value >> (offset.intValue * 8)) & 0xFF
        state.data_mem(BigInt(signedToUnsigned(address, 32)) - offset) = mem_value
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(getSignedValue(signedToUnsigned(value, 8), 32), 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
            mem_addr = signedToUnsigned(address, 32).U
            mem_rdata = signedToUnsigned(value, 32).U
            mem_rmask = 0x1.U
            mem_wdata = 0.U
            mem_wmask = 0.U
        }
        RISCVInstruction(instruction, assembly, effect, state)
    }

    def getLH(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "lh x" + rd + " " + getImmediateAsString(imm, imm_width) + "(x" + rs1 + ")"
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val address = state.registers(rs1).toLong + getSignedValue(imm, imm_width)
        val value = getRandomMemoryValue()
        val offset = BigInt(signedToUnsigned(address, 32)) % 4
        if (offset == 3) {
            // store upper byte and lower byte sperately
            val mem_value_1 = (value & 0xFF00) >>> 8
            val mem_value_2 = value & 0xFF << 24
            state.data_mem(BigInt(signedToUnsigned(address, 32)) - 3) = mem_value_2
            state.data_mem(BigInt(signedToUnsigned(address, 32)) + 1) = mem_value_1
        } else {
            val mem_value = (value >> (offset.intValue * 8)) & 0xFFFF
            state.data_mem(BigInt(signedToUnsigned(address, 32)) - offset) = mem_value
        }
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(getSignedValue(signedToUnsigned(value, 16), 32), 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
            mem_addr = signedToUnsigned(address, 32).U
            mem_rdata = signedToUnsigned(value, 32).U
            mem_rmask = 0x3.U
            mem_wdata = 0.U
            mem_wmask = 0.U
        }
        RISCVInstruction(instruction, assembly, effect, state)
    }

    def getLW(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "lw x" + rd + " " + getImmediateAsString(imm, imm_width) + "(x" + rs1 + ")"
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val address = state.registers(rs1).toLong + getSignedValue(imm, imm_width)
        val value = getRandomMemoryValue()
        val offset = BigInt(signedToUnsigned(address, 32)) % 4
        if (offset != 0) {
            val mem_value_1 = (value & 0xFF000000) >>> 24
            val mem_value_2 = (value & 0xFF0000) >>> 16
            val mem_value_3 = (value & 0xFF00) >>> 8
            val mem_value_4 = value & 0xFF
            if (offset == 1) {
                state.data_mem(BigInt(signedToUnsigned(address, 32)) - 1) = (mem_value_4 << 8) + (mem_value_3 << 16) + (mem_value_2 << 24)
                state.data_mem(BigInt(signedToUnsigned(address, 32)) + 3) = mem_value_1
            } else if (offset == 2) {
                state.data_mem(BigInt(signedToUnsigned(address, 32)) - 2) = (mem_value_4 << 16) + (mem_value_3 << 24)
                state.data_mem(BigInt(signedToUnsigned(address, 32)) + 2) = (mem_value_1 << 8) + mem_value_2
            } else {
                state.data_mem(BigInt(signedToUnsigned(address, 32)) - 3) = mem_value_4 << 24
                state.data_mem(BigInt(signedToUnsigned(address, 32)) + 1) = (mem_value_1 << 16) + (mem_value_2 << 16) + mem_value_3
            }
        } else {
            state.data_mem(BigInt(signedToUnsigned(address, 32))) = value
        }
        state.data_mem(BigInt(signedToUnsigned(address, 32))) = value
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(value, 32).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
            mem_addr = signedToUnsigned(address, 32).U
            mem_rdata = signedToUnsigned(value, 32).U
            mem_rmask = 0xF.U
            mem_wdata = 0.U
            mem_wmask = 0.U
        }
        RISCVInstruction(instruction, assembly, effect, state)
    }

    def getLBU(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "lbu x" + rd + " " + getImmediateAsString(imm, imm_width) + "(x" + rs1 + ")"
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val address = state.registers(rs1).toLong + getSignedValue(imm, imm_width)
        val value = getRandomMemoryValue()
        val offset = BigInt(signedToUnsigned(address, 32)) % 4
        val mem_value = (value >> (offset.intValue * 8)) & 0xFF
        state.data_mem(BigInt(signedToUnsigned(address, 32)) - offset) = mem_value
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(value, 8).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
            mem_addr = signedToUnsigned(address, 32).U
            mem_rdata = signedToUnsigned(value, 32).U
            mem_rmask = 0x1.U
            mem_wdata = 0.U
            mem_wmask = 0.U
        }
        RISCVInstruction(instruction, assembly, effect, state)
    }

    def getLHU(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rd = getRandomRegister(false)
        val rs1 = getRandomRegister(true)
        val instruction = "lhu x" + rd + " " + getImmediateAsString(imm, imm_width) + "(x" + rs1 + ")"
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val address = state.registers(rs1).toLong + getSignedValue(imm, imm_width)
        val value = getRandomMemoryValue()
        val offset = BigInt(signedToUnsigned(address, 32)) % 4
        if (offset == 3) {
            // store upper byte and lower byte sperately
            val mem_value_1 = (value & 0xFF00) >>> 8
            val mem_value_2 = value & 0xFF << 24
            state.data_mem(BigInt(signedToUnsigned(address, 32)) - 3) = mem_value_2
            state.data_mem(BigInt(signedToUnsigned(address, 32)) + 1) = mem_value_1
        } else {
            val mem_value = (value >> (offset.intValue * 8)) & 0xFFFF
            state.data_mem(BigInt(signedToUnsigned(address, 32)) - offset) = mem_value
        }
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = 0.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = 0.U
            rd_addr = rd.U
            rd_wdata = signedToUnsigned(value, 16).U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
            mem_addr = signedToUnsigned(address, 32).U
            mem_rdata = signedToUnsigned(value, 32).U
            mem_rmask = 0x3.U
            mem_wdata = 0.U
            mem_wmask = 0.U
        }
        RISCVInstruction(instruction, assembly, effect, state)
    }

    def getSB(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "sb x" + rs2 + " " + getImmediateAsString(imm, imm_width) + "(x" + rs1 + ")"
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val address = state.registers(rs1).toLong + getSignedValue(imm, imm_width)
        val value = state.registers(rs2).toLong & 0xFF
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = 0.U
            rd_wdata = 0.U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
            mem_addr = signedToUnsigned(address, 32).U
            mem_rdata = 0.U
            mem_rmask = 0.U
            mem_wdata = signedToUnsigned(value, 32).U
            mem_wmask = 0x1.U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSH(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "sh x" + rs2 + " " + getImmediateAsString(imm, imm_width) + "(x" + rs1 + ")"
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val address = state.registers(rs1).toLong + getSignedValue(imm, imm_width)
        val value = state.registers(rs2).toLong & 0xFFFF
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = 0.U
            rd_wdata = 0.U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
            mem_addr = signedToUnsigned(address, 32).U
            mem_rdata = 0.U
            mem_rmask = 0.U
            mem_wdata = signedToUnsigned(value, 32).U
            mem_wmask = 0x3.U
        }
        RISCVInstruction(instruction, assembly, effect)
    }

    def getSW(state: ProcessorState): RISCVInstruction = {
        val imm_width = 12
        val imm = getRandomImmediate(imm_width)
        val rs1 = getRandomRegister(true)
        val rs2 = getRandomRegister(true)
        val instruction = "sw x" + rs2 + " " + getImmediateAsString(imm, imm_width) + "(x" + rs1 + ")"
        val assembly = BigInt(RISCVAssembler.fromString(instruction).split("\n")(0), 16).U
        val address = state.registers(rs1).toLong + getSignedValue(imm, imm_width)
        val value = state.registers(rs2).toLong
        val effect = new RVFI {
            valid = true.B
            order = state.retire_count.U
            insn = assembly
            rs1_addr = rs1.U
            rs2_addr = rs2.U
            rs1_rdata = state.registers(rs1).U
            rs2_rdata = state.registers(rs2).U
            rd_addr = 0.U
            rd_wdata = 0.U
            pc_rdata = state.pc.U
            pc_wdata = (state.pc + 4).U
            mem_addr = signedToUnsigned(address, 32).U
            mem_rdata = 0.U
            mem_rmask = 0.U
            mem_wdata = signedToUnsigned(value, 32).U
            mem_wmask = 0xF.U
        }
        RISCVInstruction(instruction, assembly, effect)
    }
}
