package RISCV.implementation.RV32I

import chisel3._
import chisel3.util._

import RISCV.model.ALU_CONTROL
import RISCV.interfaces.RV32I.AbstractALU

class ALU extends AbstractALU {
    io_alu.result := 0.U
    switch (io_alu.alu_op) {
        is (ALU_CONTROL.ADD) {
            io_alu.result := io_alu.op1 +& io_alu.op2
        }
        is (ALU_CONTROL.SLL) {
            io_alu.result := io_alu.op1 << io_alu.op2(4, 0)
        }
        is (ALU_CONTROL.SLT) {
            io_alu.result := Mux(io_alu.op1.asSInt < io_alu.op2.asSInt, 1.U, 0.U)
        }
        is (ALU_CONTROL.SLTU) {
            io_alu.result := Mux(io_alu.op1 < io_alu.op2, 1.U, 0.U)
        }
        is (ALU_CONTROL.XOR) {
            io_alu.result := io_alu.op1 ^ io_alu.op2
        }
        is (ALU_CONTROL.SRL) {
            io_alu.result := io_alu.op1 >> io_alu.op2(4, 0)
        }
        is (ALU_CONTROL.OR) {
            io_alu.result := io_alu.op1 | io_alu.op2
        }
        is (ALU_CONTROL.AND) {
            io_alu.result := io_alu.op1 & io_alu.op2
        }
        is (ALU_CONTROL.SUB) {
            io_alu.result := io_alu.op1 - io_alu.op2
        }
        is (ALU_CONTROL.SRA) {
            io_alu.result := (io_alu.op1.asSInt >> io_alu.op2(4,0)).asUInt
        }
    }
}