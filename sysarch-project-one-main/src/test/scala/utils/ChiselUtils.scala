package utils

import chisel3._
import chiseltest._
import chisel3.reflect.DataMirror
import chisel3.experimental.BundleLiterals
import chisel3.experimental.hierarchy.{
  Definition,
  Instance,
  instantiable,
  public
}
import scala.util.Random
import scala.language.implicitConversions

object ChiselUtils {
  private def cat(i1: TestInt, i2: TestInt): TestInt =
    (i1.value << i2.width) + i2.value

  def cat(ints: TestInt*): TestInt = ints.reduce(cat)

  implicit def bool2int(b: Boolean): Int = if (b) 1 else 0

  implicit def bool2bigint(b: Boolean): BigInt = if (b) 1 else 0

  implicit def testint2bigint(b: TestInt): fromBigIntToLiteral = b.value

  implicit class TestInt(val value: BigInt) {
    private def this(value: BigInt, width: Int) = {
      this(value)
      this.width = width
    }

    def apply(i: Int): Boolean = value.testBit(i)

    def apply(msb: Int, lsb: Int): TestInt = {
      val width = msb - lsb + 1
      new TestInt((value >> lsb) & ((1 << width) - 1), width)
    }

    var width: Int = value.bitLength

    def ##(that: TestInt): TestInt = cat(value, that.value)

    def withWidth(newWidth: Int): TestInt = {
      new TestInt(value & ((1 << newWidth) - 1), newWidth)
    }
  }

  class RandomInputIterator(io: Bundle, r: Random)
      extends Iterator[Bundle] {

    override def hasNext: Boolean = true

    override def next(): Bundle = {
      BundleLiterals
        .AddBundleLiteralConstructor(io)
        .Lit(
          io.elements
            .filter({ case (_, data) =>
              DataMirror.specifiedDirectionOf(data) == SpecifiedDirection.Input
            })
            .map({
              case (port, data) => { bundle: Bundle =>
                (
                  bundle.elements(port),
                  data match {
                    case _: Bool => r.nextBoolean().B
                    case _: UInt => BigInt(data.getWidth, r).U
                  }
                )
              }
            })
            .toSeq: _*
        )
    }
  }

  def setRandomInput(io: Bundle, r: Random) = {
    io.getElements
      .filter(data =>
        DataMirror.specifiedDirectionOf(data) == SpecifiedDirection.Input
      )
      .foreach {
        case bool: Bool =>
          bool.poke(r.nextBoolean().B)
        case uint: UInt =>
          uint.poke(BigInt(uint.getWidth, r).U)
        case sint: SInt =>
          sint.poke((BigInt(sint.getWidth, r)-(1 << (sint.getWidth-1))).S) //quick hack
        case _ =>
      }
  }
}
