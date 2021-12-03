import DiagnosticReport._

case class BinaryNumber(asList: List[Int]):
  lazy val toDecimal: Int =
    asList.reverse.zipWithIndex
      .map {
        case (bit, index) if bit == 1 => Math.pow(2, index)
        case _                        => 0
      }
      .sum
      .toInt

case class DiagnosticReport(report: List[BinaryNumber]):

  lazy val size: Int = report.size

  lazy val matrixSum: List[Int] = report.map(_.asList).fold(Nil) { (l, r) =>
    l.zipAll(r, 0, 0).map(_ + _)
  }

  lazy val gammaRate: List[Int] =
    matrixSum.map { columnCount =>
      if (columnCount >= size - columnCount)
        1
      else
        0
    }

  lazy val epsilonRate: List[Int] = gammaRate.map(bit => Math.abs(bit - 1))

  lazy val diagnosticPower = BinaryNumber(gammaRate).toDecimal * BinaryNumber(epsilonRate).toDecimal

  lazy val oxygenGenerationRating: BinaryNumber =
    rating(this, 0)(_.gammaRate)

  lazy val carbonDioxideScrubberRating: BinaryNumber =
    rating(this, 0)(_.epsilonRate)

  lazy val lifeSupportRating: Int =
    oxygenGenerationRating.toDecimal * carbonDioxideScrubberRating.toDecimal

object DiagnosticReport:

  @scala.annotation.tailrec
  final def rating(in: DiagnosticReport, pos: Int)(
      bitCriteria: DiagnosticReport => List[Int]
  ): BinaryNumber =
    val remaining = DiagnosticReport(
      in.report.filter(_.asList.apply(pos) == bitCriteria(in).apply(pos))
    )
    if (remaining.size == 1)
      remaining.report.head
    else
      rating(remaining, pos + 1)(bitCriteria)
