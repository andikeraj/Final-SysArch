package utils

import chisel3._
import chisel3.util._
import chiseltest._

class RVFI {
    var valid = false.B
    var order = 0.U
    var insn = 0.U
    var trap = false.B
    var halt = false.B
    var intr = false.B
    var mode = 0.U
    var ixl = 0.U
    var rs1_addr = 0.U
    var rs2_addr = 0.U
    var rs1_rdata = 0.U
    var rs2_rdata = 0.U
    var rd_addr = 0.U
    var rd_wdata = 0.U
    var pc_rdata = 0.U
    var pc_wdata = 0.U
    var mem_addr = 0.U
    var mem_rmask = 0.U
    var mem_wmask = 0.U
    var mem_rdata = 0.U
    var mem_wdata = 0.U
}

