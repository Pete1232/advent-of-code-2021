import utils.Grid
import utils.BinaryNumber

import scala.Numeric.Implicits.infixNumericOps

object ImageEnhancement:
  def enhance(grid: Grid, algorithm: String, default: Int): Grid =
    val padding: Int = 3
    // step 1 is to extend the grid, since its "infinite" some non-existent pixels can be switched
    val largerGrid = grid.translate(padding, padding)
    // get the enhanced value of each point
    // extend it again, just using the points to map out the new grid - the values of this one aren't used
    largerGrid.translate(padding, padding).map { case (point, _) =>
      point -> (algorithm.apply(
        pointValue(largerGrid, point, default).toInt
      ) match
        case '#' => Some(1)
        case '.' => Some(0)
      )
    }

  def pointValue(grid: Grid, point: (Int, Int), default: Int): BinaryNumber =
    val upL = (point._1 - 1, point._2 - 1)
    val up = (point._1, point._2 - 1)
    val upR = (point._1 + 1, point._2 - 1)
    val l = (point._1 - 1, point._2)
    val r = (point._1 + 1, point._2)
    val downL = (point._1 - 1, point._2 + 1)
    val down = (point._1, point._2 + 1)
    val downR = (point._1 + 1, point._2 + 1)

    BinaryNumber(
      List(
        grid.get(upL),
        grid.get(up),
        grid.get(upR),
        grid.get(l),
        grid.get(point),
        grid.get(r),
        grid.get(downL),
        grid.get(down),
        grid.get(downR)
      ).map(_.getOrElse(default))
    )
