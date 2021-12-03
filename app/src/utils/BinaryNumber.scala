package utils

import scala.collection.immutable.Queue
import scala.math.BigInt
import scala.math.Numeric
import scala.util.Success
import scala.util.Try

case class BinaryNumber(asList: List[Int]):
  lazy val asDecimal: Int =
    asList.reverse.zipWithIndex
      .map {
        case (bit, index) if bit == 1 => Math.pow(2, index)
        case _                        => 0
      }
      .sum
      .toInt

object BinaryNumber:
  implicit def binaryNumeric: Numeric[BinaryNumber] = new Numeric[BinaryNumber]:
    def compare(x: BinaryNumber, y: BinaryNumber): Int =
      Numeric.apply[Int].compare(x.asDecimal, y.asDecimal)
    def fromInt(x: Int): BinaryNumber =
      @scala.annotation.tailrec
      def loop(divisor: Int, binary: List[Int]): List[Int] =
        if (divisor == 1) binary :+ 1
        else
          val newDivisor = x / 2
          loop(newDivisor, binary :+ (x - (newDivisor * 2)))
      BinaryNumber(loop(x, Nil))
    def minus(x: BinaryNumber, y: BinaryNumber): BinaryNumber =
      fromInt(x.toInt - y.toInt)
    def negate(x: BinaryNumber): BinaryNumber = fromInt(
      Numeric.apply[Int].negate(x.toInt)
    )
    def parseString(str: String): Option[BinaryNumber] =
      Try(str.map(_.getNumericValue).toList).toOption.map(BinaryNumber.apply)
    def plus(x: BinaryNumber, y: BinaryNumber): BinaryNumber =
      fromInt(x.toInt + y.toInt)
    def times(x: BinaryNumber, y: BinaryNumber): BinaryNumber =
      fromInt(x.toInt * y.toInt)
    def toDouble(x: BinaryNumber): Double =
      x.asList.reverse.zipWithIndex.map {
        case (bit, index) if bit == 1 => Math.pow(2, index)
        case _                        => 0
      }.sum
    def toFloat(x: BinaryNumber): Float = toDouble(x).floatValue
    def toInt(x: BinaryNumber): Int = toDouble(x).intValue
    def toLong(x: BinaryNumber): Long = toDouble(x).longValue

  def matrixSum(numbers: List[BinaryNumber]): BinaryNumber = BinaryNumber(
    numbers.map(_.asList).fold(Nil) { (l, r) =>
      l.zipAll(r, 0, 0).map(_ + _)
    }
  )
