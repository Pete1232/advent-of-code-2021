object PointsOnALine:
  def closestPoint(
      positions: List[Int]
  )(distanceFunction: (Int, Int) => Int): (Int, Int) =
    List
      .tabulate(positions.max) { point =>
        (point, distanceTo(point, positions)(distanceFunction))
      }
      .minBy(_._2)

  def distanceTo(point: Int, positions: List[Int])(
      distanceFunction: (Int, Int) => Int
  ): Int =
    positions.map(p => distanceFunction(point, p)).sum

  def simpleDistance(point1: Int, point2: Int): Int =
    Math.abs(point1 - point2)

  def complicatedDistance(point1: Int, point2: Int): Int =
    val straightLineDistance = simpleDistance(point1, point2)
    List.tabulate(straightLineDistance)(_ + 1).sum
