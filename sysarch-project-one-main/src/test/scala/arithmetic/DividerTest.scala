import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.reflect.ClassTag
import chisel3.experimental.hierarchy.{
  Definition,
  Instance,
  instantiable,
  public
}
import utils._
import chisel3.reflect.DataMirror
import scala.util.Random
import chisel3.experimental.BundleLiterals
import scala.language.implicitConversions

import arithmetic.Divider
import utils.ChiselUtils._

class DividerTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Divider"

  def divisionSpec(dividend: BigInt, divisor: BigInt, bitWidth: Int): (BigInt, BigInt) = {
    var quotient, remainder: BigInt = 0
    if (divisor != 0) {
        quotient = dividend / divisor
        remainder = dividend % divisor
    } else { 
        quotient = (1 << bitWidth)-1        //if divisor is 0, return the largest possible quotient
        remainder = dividend
    }
    (quotient, remainder)
  }

  def randomDividerTest(
    dividerGen: => Divider,
    bitWidth: Int,
    numberOfTests: Int,
    debug: Boolean
  ) = {
    test(dividerGen) { c =>
      val rand = new Random
      for (_ <- 1 to numberOfTests) {
        setRandomInput(c.io, rand)
        c.io.start.poke(true.B)
        c.clock.step()              //start the division
        c.io.start.poke(false.B)    
        while (c.io.done.peek().litToBoolean == false) {
          c.clock.step()
        }
        
        val divisor = c.io.divisor.peekInt()
        val dividend = c.io.dividend.peekInt()
        val (quotient, remainder) = divisionSpec(dividend, divisor, bitWidth)

        val actualQuotient = c.io.quotient.peekInt()
        val actualRemainder = c.io.remainder.peekInt()
        if (debug) 
          println(s"dividend: $dividend, divisor: $divisor, expected quotient: $quotient, expected remainder: $remainder", s"actual quotient: $actualQuotient, actual remainder: $actualRemainder")

        c.io.quotient.expect(quotient.U(bitWidth.W))
        c.io.remainder.expect(remainder.U(bitWidth.W))
      }
    }
  }


  "Divider" should "correctly divide two numbers" in {
    randomDividerTest(new Divider(6), 6, 1000, false)
  }
}
