package utils

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
  def matrixSum(numbers: List[BinaryNumber]): BinaryNumber = BinaryNumber(
    numbers.map(_.asList).fold(Nil) { (l, r) =>
      l.zipAll(r, 0, 0).map(_ + _)
    }
  )
