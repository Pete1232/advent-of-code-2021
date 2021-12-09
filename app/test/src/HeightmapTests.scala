import utest._
import cats.implicits._

object HeightmapTests extends TestSuite:
  val tests = Tests {
    test("build heightmap from a string") - {
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
      println(map.show)
      assert(
        map.value
          .filter(_._1._1 == 0)
          .toList
          .sortBy(_._1._2)
          .map(_._2)
          .mkString == "2199943210"
      )
    }
  }
