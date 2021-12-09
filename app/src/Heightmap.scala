import cats.Show

case class Heightmap(underlying: Map[(Int, Int), Int]):

  def isLowestPoint(point: (Int, Int)): Option[((Int, Int), Int)] =
    val up = (point._1, point._2 - 1)
    val down = (point._1, point._2 + 1)
    val left = (point._1 - 1, point._2)
    val right = (point._1 + 1, point._2)

    if (point == (9, 0))
      println(up.toString + down + left + right)

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

  def lowestPoints: Set[((Int, Int), Int)] =
    underlying.keySet.map(isLowestPoint).flatten

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
