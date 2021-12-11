import utest._
import SyntaxChecker.LineType
import utils.OctopusGrid
import cats.implicits._

object OctopusGridTests extends TestSuite:
  val tests = Tests {
    test("small example") - {
      val grid = OctopusGrid.buildGrid(
        """
        |11111
        |19991
        |19191
        |19991
        |11111
        """.stripMargin.trim
      )
      val result1 = grid.nextStep
      val result2 = result1.nextStep
      println(s"${grid.show}\n\n${result1.show}\n\n${result2.show}")
      assert(result1.flashes == 9)
      assert(result1.totalFlashes == 9)
      assert(result2.flashes == 0)
      assert(result1.totalFlashes == 9)
    }

    val testGrid = OctopusGrid.buildGrid(
      """
        |5483143223
        |2745854711
        |5264556173
        |6141336146
        |6357385478
        |4167524645
        |2176841721
        |6882881134
        |4846848554
        |5283751526
        """.stripMargin.trim
    )
    test("larger example") - {
      val resultGrid10 = testGrid.step(10)
      println(resultGrid10.show + "\n")
      val result10 = resultGrid10.totalFlashes
      assert(result10 == 204)

      val resultGrid100 = testGrid.step(100)
      println(resultGrid100.show)
      val result100 = resultGrid100.totalFlashes
      assert(result100 == 1656)
    }
    test("first simultaneous flash in larger example") - {
      val (resultGrid, resultCount) = testGrid.stepUntilAllFlashed(200).get
      println(resultGrid.show)
      assert(resultCount == 195)
    }

    test("answer") - {
      val input = OctopusGrid.buildGrid(
        """
        |7222221271
        |6463754232
        |3373484684
        |4674461265
        |1187834788
        |1175316351
        |8211411846
        |4657828333
        |5286325337
        |5771324832
        """.stripMargin.trim
      )

      val resultPart1 = input.step(100).totalFlashes
      assert(resultPart1 == 1661)

      val resultPart2 = input.stepUntilAllFlashed(1000).get._2
      assert(resultPart2 == 334)
    }
  }
