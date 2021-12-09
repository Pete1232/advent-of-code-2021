import cats.Show

case class Heightmap(underlying: Map[(Int, Int), Int]):

  def isLowestPoint(point: (Int, Int)): Option[((Int, Int), Int)] =
    val up = (point._1, point._2 - 1)
    val down = (point._1, point._2 + 1)
    val left = (point._1 - 1, point._2)
    val right = (point._1 + 1, point._2)

    val lowestPointAround = List(
      underlying.get(up),
      underlying.get(down),
      underlying.get(left),
      underlying.get(right)
    ).flatten.min

    underlying.get(point) match
      case Some(p) if p < lowestPointAround =>
        Some(point -> p)
      case _ => None

  lazy val lowestPoints: List[((Int, Int), Int)] =
    underlying.keySet.toList.map(isLowestPoint).flatten

  lazy val risk = lowestPoints.map(_._2 + 1).sum

  @scala.annotation.tailrec
  final def searchAround(
      point: (Int, Int),
      basin: Set[(Int, Int)],
      notChecked: Set[(Int, Int)]
  ): Set[(Int, Int)] =
    val up = (point._1, point._2 - 1)
    val down = (point._1, point._2 + 1)
    val left = (point._1 - 1, point._2)
    val right = (point._1 + 1, point._2)

    val basinPoints = List(
      underlying.find(_._1 == up),
      underlying.find(_._1 == down),
      underlying.find(_._1 == left),
      underlying.find(_._1 == right)
    ).flatten.filterNot(_._2 == 9).map(_._1)

    val pointsToAddToBasin = basinPoints
      .map(p =>
        if (basin.contains(p))
          None
        else
          Some(p)
      )
      .flatten ++ notChecked

    if (pointsToAddToBasin.isEmpty) basin
    else
      searchAround(
        pointsToAddToBasin.head,
        basin + pointsToAddToBasin.head,
        pointsToAddToBasin.tail.toSet
      )

object Heightmap:

  def buildHeightmap(s: String): Heightmap =
    Heightmap(buildHeightmap(s, 0, 0, Map.empty))

  @scala.annotation.tailrec
  private def buildHeightmap(
      s: String,
      x: Int,
      y: Int,
      result: Map[(Int, Int), Int]
  ): Map[(Int, Int), Int] =
    s.headOption match
      case None =>
        result
      case Some(c) if c == '\n' =>
        buildHeightmap(s.tail, 0, y + 1, result)
      case Some(c) =>
        buildHeightmap(
          s.tail,
          x + 1,
          y,
          result + ((x -> y) -> c.getNumericValue)
        )

  implicit val showHeightmap: Show[Heightmap] = Show.show(map =>
    val maxX = map.underlying.maxBy(_._1._1)._1._1
    val maxY = map.underlying.maxBy(_._1._2)._1._2
    val grid =
      List.tabulate(maxY + 1, maxX + 1)((y, x) => map.underlying((x, y)))
    grid.map(_.mkString(" ")).mkString("\n")
  )
