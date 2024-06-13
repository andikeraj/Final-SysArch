package arithmetic

import chisel3._
import chisel3.util._
import chisel3.experimental.requireIsChiselType

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

    val i = RegInit(0.U)  // set index to the length of the divident for the cycles
    io.done := false.B // set done to false since its not done 
    when (io.start){
        remainder := 0.U
        divisor := io.divisor
        i := RegInit(bitWidth.U)
        //printf(cf"${io.dividend} ${io.divisor} $remainder\n")
    }
    io.quotient := 0.U
    when (i >= 1.U) {
        val rPrime = (remainder << 1.U) + io.dividend(i-1.U) // R’ = 2 * R + A[i]
        when(rPrime < io.divisor){  
            quotient(i-1.U) := 0.U //set Q[i] to 0 since r prime is smaller than B
            remainder := rPrime
        } .otherwise{
            quotient(i-1.U) := 1.U // otherwise set it to 1
            remainder := rPrime - io.divisor
        }
        //printf(cf"$i $quotient $remainder $rPrime $divisor\n")
        i := i - 1.U // decrement i so we can continue with the other digits of the divident 
        io.done := false.B // done is false here becuase the division hasnt ended yet. we still have digits to divise in A
    }.otherwise{
        io.quotient := quotient.asUInt
        io.remainder := remainder
    io.done := true.B // done should be true here since we have reached the end of the division
}
    //output the results 
    io.quotient := quotient.asUInt 
    io.remainder := remainder
}
