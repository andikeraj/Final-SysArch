file:///C:/Users/andik/OneDrive%20-%20Universität%20des%20Saarlandes/Desktop/projekti%20i%20sysit/Final-SysArcj/sysarch-project-one-main/src/main/scala/arithmetic/Divider.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 1072
uri: file:///C:/Users/andik/OneDrive%20-%20Universität%20des%20Saarlandes/Desktop/projekti%20i%20sysit/Final-SysArcj/sysarch-project-one-main/src/main/scala/arithmetic/Divider.scala
text:
```scala
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
        i := Reginit()@@
        printf(cf"${io.dividend} ${io.divisor} $remainder\n")
    }
    io.quotient := 0.U
    when (i >= 1.U) {
        val rPrime = (remainder << 1.U) + io.dividend(i-1.U) // R’ = 2 * R + A[i]
        when(rPrime < io.divisor){
            quotient(i-1.U) := 0.U
            remainder := rPrime
        } .otherwise{
            quotient(i-1.U) := 1.U
            remainder := rPrime - io.divisor
        }
        printf(cf"$i $quotient $remainder $rPrime $divisor\n")
        i := i - 1.U
        io.done := false.B
    }.otherwise{
        io.quotient := quotient.asUInt
        io.remainder := remainder
    io.done := true.B
}
    io.quotient := quotient.asUInt
    /*for (i2 <- (0 to bitWidth-1).reverse){
        val curr_quotient = RegInit(0.U(bitWidth.W)) 
        curr_quotient := (curr_quotient<<1.U) + quotient(i2)
    }*/
    io.remainder := remainder

    // val counter = RegInit(0.U(log2Ceil(bitWidth).W)) //counter for loop
    // println(divisor)
    // when(io.start){
    //     remainder := io.remainder
    //     divisor := io.divisor
    //     quotient := 0.U
    //     counter := bitWidth.U
    // }.otherwise{
    //     when(counter =/= 0.U){
    //         val tempRemainder = (remainder << 1) | io.dividend(counter)
    //         when(tempRemainder >= divisor){
    //             remainder := tempRemainder - divisor
    //             quotient := (quotient << 1) | 1.U
    //         }.otherwise{
    //             remainder := tempRemainder
    //             quotient := quotient << 1
    //         }
    //         counter := counter - 1.U
    //     }
    // }

    // io.quotient := quotient
    // io.remainder := remainder
    // io.done := (counter === 0.U)

    
}

```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:131)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.countParams(Signatures.scala:501)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:186)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:94)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:63)
	scala.meta.internal.pc.MetalsSignatures$.signatures(MetalsSignatures.scala:17)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:51)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:412)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: 0