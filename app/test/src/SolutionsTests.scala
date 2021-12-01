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
  }
