package utils

import cats.Show
import cats.syntax.validated

case class Grid(underlying: Map[(Int, Int), Int]):

  final val columns = underlying.maxBy(_._1._1)._1._1 + 1
  final val rows = underlying.maxBy(_._1._2)._1._2 + 1

  final val size = underlying.size

  def get(point: (Int, Int)): Option[Int] =
    underlying.get(point)

  def map(f: ((Int, Int), Option[Int]) => ((Int, Int), Option[Int])): Grid =
    Grid(
      List
        .tabulate(columns + 1, rows + 1) { case point =>
          f(point, underlying.get(point))
        }
        .flatten
        .filterNot(_._2 == None)
        .toMap
        .mapValues(_.get)
        .toMap
    )

  def mapValues(f: Option[Int] => Option[Int]): Grid =
    this.map { case (point, maybeValue) =>
      point -> f(maybeValue)
    }

  def row(index: Int): Option[Map[(Int, Int), Int]] =
    val result = underlying.filterKeys(_._2 == index)
    if (result.isEmpty)
      None
    else
      Some(result.toMap)

  def column(index: Int): Option[Map[(Int, Int), Int]] =
    val result = underlying.filterKeys(_._1 == index)
    if (result.isEmpty)
      None
    else
      Some(result.toMap)

  def translate(x: Int, y: Int): Grid =
    Grid(underlying.map((k, v) => (k._1 + x, k._2 + y) -> v))

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
          result + ((x -> y) -> toNumericValue(c))
        )

  // stopgap
  // todo allow generic value type
  // todo allow this kind of mapping to be passed by the caller
  private def toNumericValue(c: Char) =
    c match
      case '#' => 1
      case '.' => 0
      case c   => c.getNumericValue

  implicit val showGrid: Show[Grid] = Show.show(map =>
    val maxX = map.columns
    val maxY = map.rows
    val grid =
      List.tabulate(maxY + 1, maxX + 1)((y, x) =>
        map.underlying.get((x, y)).getOrElse(".")
      )
    grid.map(_.mkString(" ")).mkString("\n")
  )
