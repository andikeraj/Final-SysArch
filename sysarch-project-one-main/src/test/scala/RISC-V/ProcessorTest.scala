package RISCV

import chisel3._
import chisel3.util._

import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import collection.mutable
import scala.io.Source
import java.nio.file.Paths
import java.nio.file.Path
import java.io.FileWriter
import java.io.File

import RISCV.interfaces._
import RISCV.implementation._
import RISCV.implementation.RV32I._
import RISCV.implementation.RV32M._
import RISCV.utils.assembler.RISCVAssembler
import _root_.utils.ProcessorTestUtils._
import _root_.utils._

// sbt "testOnly RISCV.ProcessorTest"

class ProcessorTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  def runProgram(folderName: String): Unit = {
    test(
      new Core(
        Seq(
          () => new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU),
          () => new MultiplicationUnit, () => new DivisionUnit()
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val programPath = Paths.get(s"src/test/resources/programs/$folderName/program.s").toAbsolutePath()
      val configPath = Paths.get(s"src/test/resources/programs/$folderName/config.json").toAbsolutePath()
      var state = new ProcessorState()

      val config: TestConfig = TestConfig.fromJson(java.nio.file.Files.readString(configPath)) match {
        case Right(value) => value
        case Left(error) => throw new Exception(error)
      }
      resetCore(dut)
      state = prepareState(dut, config.initial_reg.map(x => x._1.toInt -> x._2.toInt).toMap)
      state.data_mem = config.initial_mem
      state.instr_mem = RISCVAssembler.fromFile(programPath.toString()).split("\n").map(line => BigInt(line, 16).U).zipWithIndex.map(x=> BigInt(x._2*4 + state.pc) -> x._1.litValue).to(mutable.Map)
      for (i <- 0 until config.executed_instructions.toInt by 1) {
        val instr = state.instr_mem(state.pc)
        state = executeInstruction(dut, instr.U, state)
      }
      config.final_reg.foreach(x => assert(state.registers(x._1) == x._2))
      config.final_mem.foreach(x => assert(state.data_mem(x._1) == x._2))
    }
  }

  it should "arithmetic_public" in {
    runProgram("public/arithmetic")
  }

  it should "branch_public" in {
    runProgram("public/branch")
  }

  it should "store_public" in {
    runProgram("public/store")
  }

  it should "jal_public" in {
    runProgram("public/jal")
  }
  it should "load_word_public" in {
    runProgram("public/lw")
  }

  it should "multiply_2_5_public" in {
    runProgram("public/mul")
  }

  it should "div_public" in {
    runProgram("public/div")
  }

    it should "compute fibonacci" in {
    runProgram("public/fibonacci")
  }

  it should "compute quicksort" in {
    runProgram("public/quicksort")
  }
}
