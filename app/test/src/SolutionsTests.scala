import Solutions._
import utest._

object SolutionsTests extends TestSuite:
  val tests = Tests {
    test("sonar test") - {
      val count = Solutions.sonarScan(
        in = List(
          199, 200, 208, 210, 200, 207, 240, 269, 260, 263
        ),
        windowSize = 1
      )
      assert(count == 7)
    }

    test("sonar test with window of 3") - {
      val count = Solutions.sonarScan(
        in = List(
          199, 200, 208, 210, 200, 207, 240, 269, 260, 263
        ),
        windowSize = 3
      )
      assert(count == 5)
    }

    test("dive test") - {
      val position = Solutions.dive(
        instructions = List(
          ("forward", 5),
          ("down", 5),
          ("forward", 8),
          ("up", 3),
          ("down", 8),
          ("forward", 2)
        )
      )
      assert(position._2 == (15, 60))
      assert((position._2._1 * position._2._2) == 900)
    }

    test("binary to decimal") - {
      assert(Solutions.binaryToDecimal(List(1, 0, 1, 1, 0)) == 22)
      assert(Solutions.binaryToDecimal(List(0, 1, 0, 0, 1)) == 9)
      assert(
        Solutions.binaryToDecimal(
          List(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0)
        ) == 2322
      )
    }

    val diagnosticReport = List(
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
    )

    test("diagnostic power report") - {
      val report = Solutions.diagnosticPower(
        diagnosticReport
      )
      assert(report == 198)
    }

    test("oxygen generator report") - {
      val report = Solutions.oxygenReport(
        diagnosticReport
      )
      assert(report == 23)
    }

    test("co2 scrubber report") - {
      val report = Solutions.carbonDioxideReport(
        diagnosticReport
      )
      assert(report == 10)
    }
  }
