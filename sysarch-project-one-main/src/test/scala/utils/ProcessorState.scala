package utils

import scala.collection.mutable.Map

class ProcessorState {
    val registers = Map((0 to 31).map(i => BigInt(i) -> BigInt(0)): _*)
    var pc = 0
    var instr_mem = Map[BigInt, BigInt]()
    var data_mem = Map[BigInt, BigInt]()
    var retire_count = 0

    override def toString: String = {
        val reg_str = registers.map(x => s"x${x._1} -> ${x._2.toString(16)}").mkString("\n")
        val instr_str = instr_mem.map(x => s"${x._1.toString(16)} -> ${x._2.toString(16)}").mkString("\n")
        val data_str = data_mem.map(x => s"${x._1.toString(16)} -> ${x._2.toString(16)}").mkString("\n")
        s"Registers:\n$reg_str\n\nInstructions:\n$instr_str\n\nData:\n$data_str\n\nPC: $pc\nRetire Count: $retire_count\n"
    }
}