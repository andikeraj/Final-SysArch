package arithmetic

import chisel3._
import chisel3.util._

class Divider(bitWidth: Int) extends Module {
    val io = IO(new Bundle {
        val start = Input(Bool())
        val done = Output(Bool())
        val dividend = Input(UInt(bitWidth.W))
        val divisor = Input(UInt(bitWidth.W))
        val quotient = Output(UInt(bitWidth.W))
        val remainder = Output(UInt(bitWidth.W))
    })

    val remainder = RegInit(0.U(bitWidth.W))       //current remainder
    val quotient = RegInit(0.U(bitWidth.W))        //quotient
    val divisor = RegInit(0.U(bitWidth.W))         //divisor
    val counter = RegInit(0.U(log2Ceil(bitWidth).W)) //counter for loop

    when(io.start){
        remainder := io.dividend
        divisor := io.divisor
        quotient := 0.U
        counter := bitWidth.U
    }.otherwise{
        when(counter =/= 0.U){
            val tempRemainder = (remainder << 1) | io.dividend(counter)
            when(tempRemainder >= divisor){
                remainder := tempRemainder - divisor
                quotient := (quotient << 1) | 1.U
            }.otherwise{
                remainder := tempRemainder
                quotient := quotient << 1
            }
            counter := counter - 1.U
        }
    }

    io.quotient := quotient
    io.remainder := remainder
    io.done := (counter === 0.U)
}
