package utils

import cats.Show

case class Grid(underlying: Map[(Int, Int), Int]):

  def transposeY(yValue: Int): Grid =
    Grid(
      underlying.foldLeft(underlying) { (currentState, nextPointAndValue) =>
        if (nextPointAndValue._1._2 <= yValue)
          currentState
        else
          currentState
            .removed(nextPointAndValue._1)
            .updatedWith(
              Grid.transposePoint(
                nextPointAndValue._1,
                x = None,
                y = Some(yValue)
              )
            )(v => Some(v.getOrElse(0) + nextPointAndValue._2))
      }
    )

  def transposeX(xValue: Int): Grid =
    Grid(
      underlying.foldLeft(underlying) { (currentState, nextPointAndValue) =>
        if (nextPointAndValue._1._1 <= xValue)
          currentState
        else
          currentState
            .removed(nextPointAndValue._1)
            .updatedWith(
              Grid.transposePoint(
                nextPointAndValue._1,
                x = Some(xValue),
                y = None
              )
            )(v => Some(v.getOrElse(0) + nextPointAndValue._2))
      }
    )

object Grid:
  def buildGrid(s: String): Grid =
    Grid(buildGrid(s, 0, 0, Map.empty))

  def transposePoint(
      point: (Int, Int),
      x: Option[Int],
      y: Option[Int]
  ): (Int, Int) =
    (x, y) match
      case (None, Some(yValue)) =>
        // horizontal line
        (point._1, yValue + yValue - point._2)
      case (Some(xValue), None) =>
        // vertical line
        (xValue + xValue - point._1, point._2)
      case _ =>
        ???

  @scala.annotation.tailrec
  private def buildGrid(
      s: String,
      x: Int,
      y: Int,
      result: Map[(Int, Int), Int]
  ): Map[(Int, Int), Int] =
    s.headOption match
      case None =>
        result
      case Some(c) if c == '\n' =>
        buildGrid(s.tail, 0, y + 1, result)
      case Some(c) =>
        buildGrid(
          s.tail,
          x + 1,
          y,
          result + ((x -> y) -> c.getNumericValue)
        )

  implicit val showGrid: Show[Grid] = Show.show(map =>
    val maxX = map.underlying.maxBy(_._1._1)._1._1
    val maxY = map.underlying.maxBy(_._1._2)._1._2
    val grid =
      List.tabulate(maxY + 1, maxX + 1)((y, x) =>
        map.underlying.get((x, y)).getOrElse(".")
      )
    grid.map(_.mkString(" ")).mkString("\n")
  )
