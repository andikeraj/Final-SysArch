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
    val quotient = RegInit(VecInit(Seq.fill(bitWidth)(0.U(1.W))))   //= {dividend[i:0], quotient[N−1:i+1]}, where dividend is the input dividend and quotient is the final output quotient, and i is the current cycle
    val divisor = RegInit(0.U(bitWidth.W))         //divisor


    // bit_acc - to store the current bit 
    // new_rem - to store the remainder after 

    
    when(io.start){
        //when(divisor == 0){
        //    throw new ArithmeticException("division by 0") 
        //}

        val bit_acc = RegInit(0.U)
        val new_rem = RegInit(0.U(bitWidth.W))
        val n = bitWidth

        

        for (i <- (n-1) until 0){
            when(new_rem < divisor){
                quotient(i) := 0.U
                remainder := new_rem
            }.otherwise{
                quotient(i) := 1.U
                remainder := new_rem - divisor
            }
        }
    }
    
    io.done := true.B
}
