import Solutions._
import utest._

object SolutionsTests extends TestSuite:
  val tests = Tests {
    test("sonar test") - {
      val count = Solutions.sonarScan(
        List(
          199, 200, 208, 210, 200, 207, 240, 269, 260, 263
        )
      )
      assert(count == 7)
    }
  }
