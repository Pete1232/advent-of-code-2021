object PointsOnALine:
  def closestPoint(positions: List[Int]): (Int, Int) =
    List.tabulate(positions.max) { point =>
      (point, distanceTo(point, positions))
    }.minBy(_._2)

  def distanceTo(point: Int, positions: List[Int]): Int =
    positions.map(p => Math.abs(point - p)).sum