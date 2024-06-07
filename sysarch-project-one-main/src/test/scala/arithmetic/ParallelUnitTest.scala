import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.util.Random

import arithmetic._

class AdderComputationalUnit(width: Int) extends ComputationalUnit(width) {
    io.c := io.a + io.b
}

class ParallelUnitTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
    behavior of "Parallel Unit"

    def assertEqualModuloTwoToN(a: BigInt, b: BigInt, n: Int): Unit = {
        assert((a - b) % (1 << n) == 0)
    }

    def printDebug(m: String, output: Boolean) = {
        if (output) {
            print(m)
        }
    }

    def testParallelUnit(c: ParallelUnit, a: Seq[BigInt], b: Seq[BigInt], res: Seq[BigInt], width: Int, debug: Boolean) = {
        printDebug("a: ", debug)
        c.io.a.zip(a).foreach { case (toAssign, value) => toAssign.poke(value.U) ; printDebug(value.toString+ " ", debug) }
        printDebug("\nb: ", debug)
        c.io.b.zip(b).foreach { case (toAssign, value) => toAssign.poke(value.U) ; printDebug(value.toString+ " ", debug) }

        c.io.start.poke(true.B)
        c.clock.step()
        c.io.start.poke(false.B)
        while (!c.io.done.peekBoolean()) {
            c.clock.step()
            printDebug("\nclock", debug)
        }

        printDebug("\nres: ", debug)
        c.io.c.zip(res).foreach { case (toTest, value) =>  printDebug(value.toString + " ", debug); assertEqualModuloTwoToN(toTest.peekInt(), value, width) }
        printDebug("\ndone\n", debug)
    }



    it should "add two vectors" in {
        val width = 8
        val vectorSize = 12
        val arraySize = 4
        test(new ParallelUnit(vectorSize, arraySize, width, new AdderComputationalUnit(_))) { dut =>
            val rand = new Random
            for (_ <- 1 to 50) {
                val a = Seq.fill(vectorSize)(BigInt(width, rand))
                val b = Seq.fill(vectorSize)(BigInt(width, rand))
                val res = a.zip(b).map[BigInt]{ case (a, b) => a+b }
                
                testParallelUnit(dut, a, b, res, width, false)
            }
        }
    }
        
}
