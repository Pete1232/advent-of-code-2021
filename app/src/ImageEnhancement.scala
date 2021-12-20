import utils.Grid
import utils.BinaryNumber

import scala.Numeric.Implicits.infixNumericOps

case class Image(grid: Grid):
  def enhance(algorithm: String): Image =
    // step 1 is to extend the grid, since its "infinite" some non-existent pixels can be switched
    val largerGrid = grid.translate(1, 1)
    // get the enhanced value of each point
    Image(largerGrid.map { case (point, maybeValue) =>
      point -> (algorithm.apply(pointValue(point).toInt) match
        case '#' => Some(1)
        case '.' => None
      )
    })

  def pointValue(point: (Int, Int)): BinaryNumber =
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
      ).map(_.getOrElse(0))
    )
