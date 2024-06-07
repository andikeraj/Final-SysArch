package RISCV.implementation.RV32I

import chisel3._
import chisel3.util._

import RISCV.interfaces.RV32I.AbstractBranchUnit
import RISCV.model.RISCV_TYPE

class BranchUnit  extends AbstractBranchUnit{
    io_branch.branch_taken := false.B
  switch (io_branch.instr_type) {
    is (RISCV_TYPE.beq) {
        io_branch.branch_taken := io_branch.alu_out === 0.U
    }
    is (RISCV_TYPE.bne) {
        io_branch.branch_taken := io_branch.alu_out =/= 0.U
    }
    is (RISCV_TYPE.blt) {
        io_branch.branch_taken := io_branch.alu_out === 1.U
    }
    is (RISCV_TYPE.bge) {
        io_branch.branch_taken := io_branch.alu_out === 0.U
    }
    is (RISCV_TYPE.bltu) {
        io_branch.branch_taken := io_branch.alu_out === 1.U
    }
    is (RISCV_TYPE.bgeu) {
        io_branch.branch_taken := io_branch.alu_out === 0.U
    }
  }
}
