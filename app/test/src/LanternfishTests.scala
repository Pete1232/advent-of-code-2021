import Solutions._
import utest._

object LanternfishTests extends TestSuite:
  val tests = Tests {
    val fish = List(3, 4, 3, 1, 2)
    val initialDistribution = Lanternfish.countFish(fish)
    test("count lanternfish for 18 days") - {
      val count =
        Lanternfish
          .nextDays(18, initialDistribution)
          .values
          .sum
      assert(count == 26)
    }
    test("count lanternfish for 80 days") - {
      val count =
        Lanternfish
          .nextDays(80, initialDistribution)
          .values
          .sum
      assert(count == 5934)
    }
    test("count lots of fish") - {
      val fish = List(
        1, 4, 3, 3, 1, 3, 1, 1, 1, 2, 1, 1, 1, 4, 4, 1, 5, 5, 3, 1, 3, 5, 2, 1,
        5, 2, 4, 1, 4, 5, 4, 1, 5, 1, 5, 5, 1, 1, 1, 4, 1, 5, 1, 1, 1, 1, 1, 4,
        1, 2, 5, 1, 4, 1, 2, 1, 1, 5, 1, 1, 1, 1, 4, 1, 5, 1, 1, 2, 1, 4, 5, 1,
        2, 1, 2, 2, 1, 1, 1, 1, 1, 5, 5, 3, 1, 1, 1, 1, 1, 4, 2, 4, 1, 2, 1, 4,
        2, 3, 1, 4, 5, 3, 3, 2, 1, 1, 5, 4, 1, 1, 1, 2, 1, 1, 5, 4, 5, 1, 3, 1,
        1, 1, 1, 1, 1, 2, 1, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1,
        1, 1, 1, 1, 1, 4, 5, 1, 3, 1, 4, 4, 2, 3, 4, 1, 1, 1, 5, 1, 1, 1, 4, 1,
        5, 4, 3, 1, 5, 1, 1, 1, 1, 1, 5, 4, 1, 1, 1, 4, 3, 1, 3, 3, 1, 3, 2, 1,
        1, 3, 1, 1, 4, 5, 1, 1, 1, 1, 1, 3, 1, 4, 1, 3, 1, 5, 4, 5, 1, 1, 5, 1,
        1, 4, 1, 1, 1, 3, 1, 1, 4, 2, 3, 1, 1, 1, 1, 2, 4, 1, 1, 1, 1, 1, 2, 3,
        1, 5, 5, 1, 4, 1, 1, 1, 1, 3, 3, 1, 4, 1, 2, 1, 3, 1, 1, 1, 3, 2, 2, 1,
        5, 1, 1, 3, 2, 1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 5, 1, 1, 1, 1,
        3, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1
      )
      val dist = Lanternfish.countFish(fish)
      val count =
        Lanternfish
          .nextDays(
            80,
            dist
          )
          .values
          .sum
      assert(count == 379114)
    }
  }
