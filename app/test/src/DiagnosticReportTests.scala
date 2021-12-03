import Solutions._
import utest._
import utils.BinaryNumber

object DiagnosticReportTests extends TestSuite:
  val tests = Tests {

    val diagnosticReport = DiagnosticReport(
      List(
        List(0, 0, 1, 0, 0),
        List(1, 1, 1, 1, 0),
        List(1, 0, 1, 1, 0),
        List(1, 0, 1, 1, 1),
        List(1, 0, 1, 0, 1),
        List(0, 1, 1, 1, 1),
        List(0, 0, 1, 1, 1),
        List(1, 1, 1, 0, 0),
        List(1, 0, 0, 0, 0),
        List(1, 1, 0, 0, 1),
        List(0, 0, 0, 1, 0),
        List(0, 1, 0, 1, 0)
      ).map(BinaryNumber.apply)
    )

    test("diagnostic power report") - {
      val report = diagnosticReport.diagnosticPower
      assert(report == 198)
    }

    test("oxygen generator report") - {
      val report = diagnosticReport.oxygenGenerationRating.asDecimal
      assert(report == 23)
    }

    test("co2 scrubber report") - {
      val report = diagnosticReport.carbonDioxideScrubberRating.asDecimal
      assert(report == 10)
    }

    test("life support report") - {
      val report = diagnosticReport.lifeSupportRating
      assert(report == 230)
    }
  }
