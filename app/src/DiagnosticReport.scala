import DiagnosticReport._
import utils.BinaryNumber

case class DiagnosticReport(report: List[BinaryNumber]):

  lazy val size: Int = report.size

  lazy val gammaRate: BinaryNumber =
    BinaryNumber(BinaryNumber.matrixSum(report).asList.map { columnCount =>
      if (columnCount >= size - columnCount)
        1
      else
        0
    })

  lazy val epsilonRate: BinaryNumber = BinaryNumber(
    gammaRate.asList.map(bit => Math.abs(bit - 1))
  )

  lazy val diagnosticPower: Int =
    gammaRate.asDecimal * epsilonRate.asDecimal

  lazy val oxygenGenerationRating: BinaryNumber =
    rating(this, 0)(_.gammaRate)

  lazy val carbonDioxideScrubberRating: BinaryNumber =
    rating(this, 0)(_.epsilonRate)

  lazy val lifeSupportRating: Int =
    oxygenGenerationRating.asDecimal * carbonDioxideScrubberRating.asDecimal

object DiagnosticReport:

  @scala.annotation.tailrec
  final def rating(in: DiagnosticReport, pos: Int)(
      bitCriteria: DiagnosticReport => BinaryNumber
  ): BinaryNumber =
    val remaining = DiagnosticReport(
      in.report.filter(_.asList.apply(pos) == bitCriteria(in).asList.apply(pos))
    )
    if (remaining.size == 1)
      remaining.report.head
    else
      rating(remaining, pos + 1)(bitCriteria)
