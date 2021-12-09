import cats.Show

case class Heightmap(value: Map[(Int, Int), Int])

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
        buildHeightmap(s.tail, x+ 1, 0, result)
      case Some(c) =>
        buildHeightmap(s.tail, x, y + 1, result + ((x -> y) -> c.getNumericValue))

  implicit val showHeightmap: Show[Heightmap] = Show.show(map =>
    val maxX = map.value.maxBy(_._1._1)._1._1
    val maxY = map.value.maxBy(_._1._2)._1._2
    val grid = List.tabulate(maxX + 1, maxY + 1)((x, y) => map.value((x, y)))
    grid.map(_.mkString(" ")).mkString("\n")
  )
