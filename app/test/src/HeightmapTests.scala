import utest._
import cats.implicits._

object HeightmapTests extends TestSuite:
  val tests = Tests {
    val map = Heightmap
      .buildHeightmap(
        """
          |2199943210
          |3987894921
          |9856789892
          |8767896789
          |9899965678
          """.stripMargin.trim
      )
    test("build heightmap from a string") - {
      val result = map.underlying
        .filter(_._1._2 == 0)
        .toList
        .sortBy(_._1._1)
        .map(_._2)
        .mkString
      println(map.show)
      assert(
        result == "2199943210"
      )
    }
    test("(1,0) is a lowest point") - {
      assert(map.isLowestPoint((1, 0)).isDefined == true)
    }
    test("(9,0) is a lowest point") - {
      assert(map.isLowestPoint((9, 0)).isDefined == true)
    }
    test("(2,2) is a lowest point") - {
      assert(map.isLowestPoint((2, 2)).isDefined == true)
    }
    test("(6,4) is a lowest point") - {
      assert(map.isLowestPoint((6, 4)).isDefined == true)
    }
    test("find the lowest points") - {
      val result = map.lowestPoints.map(_._2)
      assert(result == Set(1, 0, 5, 5))
    }
  }
