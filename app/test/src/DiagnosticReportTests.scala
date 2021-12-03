import Solutions._
import utest._

object DiagnosticReportTests extends TestSuite:
  val tests = Tests {
    test("binary to decimal") - {
      assert(BinaryNumber(List(1, 0, 1, 1, 0)).toDecimal == 22)
      assert(BinaryNumber(List(0, 1, 0, 0, 1)).toDecimal == 9)
      assert(
        BinaryNumber(
          List(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0)
        ).toDecimal == 2322
      )
    }

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
      val report = diagnosticReport.oxygenGenerationRating.toDecimal
      assert(report == 23)
    }

    test("co2 scrubber report") - {
      val report = diagnosticReport.carbonDioxideScrubberRating.toDecimal
      assert(report == 10)
    }

    test("life support report") - {
      val report = diagnosticReport.lifeSupportRating
      assert(report == 230)
    }
  }
