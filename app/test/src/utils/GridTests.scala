package utils

import utest._
import cats.implicits._

object GridTests extends TestSuite:
  val tests = Tests {

    val testGrid = Grid(
      Map(
        (6, 10) -> 1,
        (0, 14) -> 1,
        (9, 10) -> 1,
        (0, 3) -> 1,
        (10, 4) -> 1,
        (4, 11) -> 1,
        (6, 0) -> 1,
        (6, 12) -> 1,
        (4, 1) -> 1,
        (0, 13) -> 1,
        (10, 12) -> 1,
        (3, 4) -> 1,
        (3, 0) -> 1,
        (8, 4) -> 1,
        (1, 10) -> 1,
        (2, 14) -> 1,
        (8, 10) -> 1,
        (9, 0) -> 1
      )
    )

    "count rows and columns" - {
      val rows = testGrid.rows
      val cols = testGrid.columns
      assert(rows == 15)
      assert(cols == 11)
    }

    "return a row" - {
      val row = testGrid.row(4)
      assert(
        row == Some(
          Map(
            (3, 4) -> 1,
            (8, 4) -> 1,
            (10, 4) -> 1
          )
        )
      )

      val noRow = testGrid.row(50)
      assert(noRow == None)
    }

    "return a column" - {
      val col = testGrid.column(3)
      assert(
        col == Some(
          Map(
            (3, 0) -> 1,
            (3, 4) -> 1
          )
        )
      )

      val noCol = testGrid.row(50)
      assert(noCol == None)
    }

    "map" - {
      val result = testGrid.map { case (point, maybeValue) =>
        point -> Some(point._1 / 10)
      }
      assert(result.underlying.values.sum == 15)
    }

    "mapValues" - {
      val result1 = testGrid.mapValues(identity)
      assert(result1 == testGrid)

      val result2 = testGrid.mapValues(_ => Some(1))
      assert(result2.underlying.values.sum == 165)
    }

    "transpose a point through a horizontal line" - {
      val result1 = Grid.transposePoint((0, 14), x = None, y = Some(7))
      assert(result1 == (0, 0))

      val result2 = Grid.transposePoint((5, 7), x = None, y = Some(7))
      assert(result2 == (5, 7))

      val result3 = Grid.transposePoint((3, 4), x = None, y = Some(7))
      assert(result3 == (3, 10))
    }

    "transpose a grid once along a horizontal line and once along a vertical line" - {
      println(testGrid.show + "\n")
      val newGrid = testGrid.transposeY(yValue = 7)
      println(newGrid.show + "\n")
      assert(newGrid.underlying.size == 17)

      val newGrid2 = newGrid.transposeX(xValue = 5)
      println(newGrid2.show)
      assert(newGrid2.underlying.size == 16)
    }
  }
