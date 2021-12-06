import Solutions._
import utest._

object LanternfishTests extends TestSuite:
  val tests = Tests {
    test("count lanternfish for 18 days") - {
      val initialDistribution = Map(
        1 -> 1,
        2 -> 1,
        3 -> 2,
        4 -> 1
      )
      val count =
        Lanternfish
          .nextDays(18, initialDistribution)
          .values
          .sum
      assert(count == 26)
    }
    test("count lanternfish for 80 days") - {
      val initialDistribution = Map(
        1 -> 1,
        2 -> 1,
        3 -> 2,
        4 -> 1
      )
      val count =
        Lanternfish
          .nextDays(80, initialDistribution)
          .values
          .sum
      assert(count == 5934)
    }
  }
