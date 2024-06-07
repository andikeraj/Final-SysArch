package utils

import scala.util.Random

import chisel3._
import chisel3.util._
import chiseltest._

import scala.collection.mutable

import RISCV.utils.assembler.RISCVAssembler
import RISCV.implementation.generic.GenericCore

object ProcessorTestUtils {
    def resetCore(core: GenericCore) : Unit = {
        core.io_reset.boot_addr.poke(0.U)
        core.io_reset.rst_n.poke(false.B)
        core.io_instr.instr_gnt.poke(false.B)
        core.io_data.data_gnt.poke(false.B)
        core.clock.step()
        core.io_reset.rst_n.poke(true.B)
        core.clock.step()
    }

    def executeInstruction(core: GenericCore, instruction: UInt) : Unit = {
        core.io_instr.instr_rdata.poke(instruction)
        core.io_instr.instr_gnt.poke(true.B)
        core.clock.step()
    }

    def evaluateRVFI(core: GenericCore, expected_state: RVFI) : Unit = {
        core.io_rvfi.rvfi_valid.expect(expected_state.valid)
        core.io_rvfi.rvfi_order.expect(expected_state.order)
        core.io_rvfi.rvfi_insn.expect(expected_state.insn)
        core.io_rvfi.rvfi_trap.expect(expected_state.trap)
        core.io_rvfi.rvfi_halt.expect(expected_state.halt)
        core.io_rvfi.rvfi_intr.expect(expected_state.intr)
        core.io_rvfi.rvfi_mode.expect(expected_state.mode)
        core.io_rvfi.rvfi_ixl.expect(expected_state.ixl)

        core.io_rvfi.rvfi_rs1_addr.expect(expected_state.rs1_addr)
        core.io_rvfi.rvfi_rs2_addr.expect(expected_state.rs2_addr)
        core.io_rvfi.rvfi_rs1_rdata.expect(expected_state.rs1_rdata)
        core.io_rvfi.rvfi_rs2_rdata.expect(expected_state.rs2_rdata)

        core.io_rvfi.rvfi_rd_addr.expect(expected_state.rd_addr)
        core.io_rvfi.rvfi_rd_wdata.expect(expected_state.rd_wdata)

        core.io_rvfi.rvfi_pc_rdata.expect(expected_state.pc_rdata)
        core.io_rvfi.rvfi_pc_wdata.expect(expected_state.pc_wdata)

        core.io_rvfi.rvfi_mem_addr.expect(expected_state.mem_addr)
        core.io_rvfi.rvfi_mem_rmask.expect(expected_state.mem_rmask)
        core.io_rvfi.rvfi_mem_wmask.expect(expected_state.mem_wmask)
        core.io_rvfi.rvfi_mem_rdata.expect(expected_state.mem_rdata)
        core.io_rvfi.rvfi_mem_wdata.expect(expected_state.mem_wdata)
    }

    def expectMemoryRequest(core: GenericCore, expected_state: RVFI) : Unit = {
        core.io_rvfi.rvfi_valid.expect(false.B)
        core.io_data.data_req.expect(true.B)
        core.io_data.data_we.expect(expected_state.mem_wmask.litValue != 0)
        core.io_data.data_be.expect(expected_state.mem_rmask.litValue | expected_state.mem_wmask.litValue)
        core.io_data.data_addr.expect(expected_state.mem_addr)
        core.io_data.data_wdata.expect(expected_state.mem_wdata)
        core.io_data.data_rdata.poke(expected_state.mem_rdata)
        core.io_data.data_gnt.poke(expected_state.mem_rmask.litValue != 0)
        core.clock.step()
    }

    def executeAndEvaluate(core: GenericCore, instruction: UInt, expected_state: RVFI) : Unit = {
        executeInstruction(core, instruction)
        evaluateRVFI(core, expected_state)
    }

    def executeAndEvaluateMemory(core: GenericCore, instruction: UInt, expected_state: RVFI) : Unit = {
        executeInstruction(core, instruction)
        expectMemoryRequest(core, expected_state)
        evaluateRVFI(core, expected_state)
    }

    def generateRandomMap(limit: Int): Map[Int, Int] = Random.shuffle((1 to 31).toList).map(i => i -> Random.nextInt(limit)).toMap

    def prepareState(core: GenericCore, registers: Map[Int,Int]) : ProcessorState = {
        var state = new ProcessorState()
        var instructions = registers.map { case (k, v) => {java.lang.Long.parseLong(RISCVAssembler.fromString("addi x" + k + " x0 0x" + v.toHexString).split("\n")(0), 16).U }}
        instructions.foreach(instr => state = executeInstruction(core, instr, state))
        return state
    }

    def repeatBits(input: Int): Int = (0 until 4).map(i => ((input >> i) & 1) * 0xFF << (i * 8)).reduce(_ | _)

    def load(state: ProcessorState, address: BigInt, mask: BigInt): BigInt = {
        // Initialize a result accumulator
        var result: BigInt = 0
        var shiftAmount = 0

        for (i <- 0 until 4) {
            // Check if the current byte needs to be read based on the mask
            if ((mask & (0xFF << (i * 8))) != 0) {
                // Calculate the byte address (address + i) and read the byte
                val byteAddress = address + i
                val offset = (byteAddress % 4).toInt
                val aligned_address = byteAddress - offset
                val word = state.data_mem(aligned_address)
                val byte = (word >> (offset * 8)) & 0xFF
                result |= (byte & 0xFF) << shiftAmount
            }
            shiftAmount += 8
        }
        return result
    }

    def store(state: ProcessorState, address: BigInt, mask: BigInt, data: BigInt): mutable.Map[BigInt, BigInt] = {
        for (i <- 0 until 4) {
            // Check if the current byte needs to be written based on the mask
            if ((mask & (0xFF << (i * 8))) != 0) {
                // Calculate the byte address (address + i) and read the byte
                val byteAddress = address + i
                val offset = (byteAddress % 4).toInt
                val aligned_address = byteAddress - offset
                val word = state.data_mem.get(aligned_address).getOrElse(BigInt(0))
                val byte = (data >> (i * 8)) & 0xFF
                val new_word = (word & ~(0xFF << (offset * 8))) | (byte << (offset * 8))
                state.data_mem += (aligned_address -> new_word)
            }
        }
        return state.data_mem
    }

    def executeInstruction(core: GenericCore, instruction: UInt, previous_state: ProcessorState) : ProcessorState = {
        core.io_instr.instr_rdata.poke(instruction)
        core.io_instr.instr_gnt.poke(true.B)
        core.clock.step()
        var had_data_request = false;
        var had_read_request = false;
        var had_write_request = false;
        var read_address: BigInt = 0;
        var write_address: BigInt = 0;
        var read_mask: BigInt = 0;
        var write_mask: BigInt = 0;
        var read_data: BigInt = 0;
        var write_data: BigInt = 0;
        while (!core.io_rvfi.rvfi_valid.peek().litToBoolean) {
            if (core.io_instr.instr_req.peek().litToBoolean) {
                core.io_instr.instr_gnt.poke(true.B)
                core.io_instr.instr_rdata.poke(instruction)
            } else {
                core.io_instr.instr_gnt.poke(false.B)
                core.io_instr.instr_rdata.poke(instruction)
            }
            if (core.io_data.data_req.peek().litToBoolean) {
                had_data_request = true
                val mask = repeatBits(core.io_data.data_be.peek().litValue.toInt)
                if (!core.io_data.data_we.peek().litToBoolean) {
                    had_read_request = true
                    read_address = core.io_data.data_addr.peek().litValue
                    read_mask = core.io_data.data_be.peek().litValue
                    read_data = load(previous_state, read_address, mask)
                    core.io_data.data_rdata.poke(read_data.U)
                } else {
                    had_write_request = true
                    write_address = core.io_data.data_addr.peek().litValue
                    write_mask = core.io_data.data_be.peek().litValue
                    write_data = core.io_data.data_wdata.peek().litValue
                    previous_state.data_mem = store(previous_state, write_address, mask, write_data)
                }
                core.io_data.data_gnt.poke(true.B)
            } else {
                core.io_data.data_gnt.poke(false.B)
            }
            core.clock.step()
        }
        core.io_instr.instr_gnt.poke(false.B)
        core.io_data.data_gnt.poke(false.B)
        core.io_rvfi.rvfi_valid.expect(true.B)
        core.io_rvfi.rvfi_order.expect(previous_state.retire_count)
        previous_state.retire_count += 1
        core.io_rvfi.rvfi_insn.expect(instruction)
        core.io_rvfi.rvfi_halt.expect(false.B)
        core.io_rvfi.rvfi_trap.expect(false.B)
        core.io_rvfi.rvfi_intr.expect(false.B)
        core.io_rvfi.rvfi_mode.expect(0.U)
        core.io_rvfi.rvfi_ixl.expect(0.U)

        core.io_rvfi.rvfi_pc_rdata.expect(previous_state.pc.U)
        previous_state.pc = core.io_rvfi.rvfi_pc_wdata.peek().litValue.toInt

        core.io_rvfi.rvfi_rs1_rdata.expect(previous_state.registers(core.io_rvfi.rvfi_rs1_addr.peek().litValue.toInt).U)
        core.io_rvfi.rvfi_rs2_rdata.expect(previous_state.registers(core.io_rvfi.rvfi_rs2_addr.peek().litValue.toInt).U)
        previous_state.registers += (core.io_rvfi.rvfi_rd_addr.peek().litValue -> core.io_rvfi.rvfi_rd_wdata.peek().litValue)

        if (had_read_request) {
            core.io_rvfi.rvfi_mem_addr.expect(read_address.U)
            core.io_rvfi.rvfi_mem_rmask.expect(read_mask.U)
            core.io_rvfi.rvfi_mem_rdata.expect(read_data.U)
        } else {
            core.io_rvfi.rvfi_mem_rmask.expect(0.U)
            core.io_rvfi.rvfi_mem_rdata.expect(0.U)
        }
        
        if (had_write_request) {
            core.io_rvfi.rvfi_mem_addr.expect(write_address.U)
            core.io_rvfi.rvfi_mem_wmask.expect(write_mask.U)
            core.io_rvfi.rvfi_mem_wdata.expect(write_data.U)
        } else {
            core.io_rvfi.rvfi_mem_wmask.expect(0.U)
            core.io_rvfi.rvfi_mem_wdata.expect(0.U)
        }

        if (!had_data_request) {
            core.io_rvfi.rvfi_mem_addr.expect(0.U)
        }
        return previous_state
    }
}
