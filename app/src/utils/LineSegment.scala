package utils

case class LineSegment(point1: (Int, Int), point2: (Int, Int)):
  val (start, end) =
    if (point1._1 <= point2._1)
      (point1, point2)
    else
      (point2, point1)
  val height = end._2 - start._2
  val width = end._1 - start._1
  val gradient =
    if (width == 0)
      if (height >= 0)
        1
      else
        -1
    else
      height / width
  val points = nextPoint(points = List(start))

  @scala.annotation.tailrec
  final def nextPoint(points: List[(Int, Int)]): List[(Int, Int)] =
    val x = points.last._1
    val y = points.last._2
    if (Math.abs(gradient) != 1) // not a vertical line
      if (x == end._1)
        points
      else
        nextPoint(points = points :+ (x + 1, y + (gradient * x)))
    else if (y == end._2)
      points
    else
      nextPoint(points = points :+ (x, y + gradient))

object LineSegment:
  def getPointsFrequency(
      lineSegments: LineSegment*
  ): Map[(Int, Int), Int] =
    val result = lineSegments
      .filter(line =>
        List(-1, 0, 1).contains(line.gradient)
      ) // only horizontal and vertical lines for part 1
      .flatMap(line => line.points)
      .foldLeft(Map.empty[(Int, Int), Int])((map, point) =>
        val currentCount = map.get(point).getOrElse(0)
        map + (point -> (currentCount + 1))
      )
    renderPointsFrequency(result)
    result

  private def renderPointsFrequency(frequency: Map[(Int, Int), Int]): Unit =
    val maxX = frequency.keySet.maxBy((x, y) => x)._1
    val maxY = frequency.keySet.maxBy((x, y) => y)._2
    val grid =
      List
        .tabulate(maxX + 1, maxY + 1)((x, y) =>
          frequency.get((x, y)).map(_.toString).getOrElse('.')
        )
        .transpose
    grid.foreach(row =>
      row.foreach(count => print(count.toString + " "))
      println("")
    )
