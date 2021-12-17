case class Target(xMin: Int, xMax: Int, yMin: Int, yMax: Int):
  require(xMin < xMax)
  require(yMin < yMax && yMax < 0)

  // more than this will drop below the minimum y position
  val minYVelocity: Int = yMin
  val maxYVelocity: Int = Math.abs(yMin)

  // more than this will overshoot the maximum x position
  val maxXVelocity: Int = xMax

  // the target area
  val xRange = Range.inclusive(xMin, xMax)
  val yRange = Range.inclusive(yMin, yMax)

  // take a trick shot at the target
  // returns the point it hits the target area... if it does
  @scala.annotation.tailrec
  final def trickShot(
      start: (Int, Int),
      xVelocity: Int,
      yVelocity: Int
  ): Option[(Int, Int)] =
    val nextX = start._1 + xVelocity
    val nextY = start._2 + yVelocity
    if (xRange.contains(nextX) && yRange.contains(nextY))
      Some(nextX -> nextY)
    else if (nextX < xMax && nextY > yMin)
      trickShot(
        nextX -> nextY,
        xVelocity = Math.max(xVelocity - 1, 0),
        yVelocity = yVelocity - 1
      )
    else
      None

  // find the correct x velocity to make a trick shot for a given y velocity
  @scala.annotation.tailrec
  final def findXVelocity(xVelocity: Int = 0, yVelocity: Int): Option[Int] =
    if (xVelocity < xMax)
      trickShot(
        start = 0 -> 0,
        xVelocity = xVelocity,
        yVelocity = yVelocity
      ) match {
        case None =>
          findXVelocity(xVelocity + 1, yVelocity)
        case _ => Some(xVelocity)
      }
    else
      None

  // returns the height of the shot that goes the highest
  lazy val bestShot: Option[Int] =
    val maxHeights = Range
      .inclusive(yMin, Math.abs(yMin))
      .map(y => y -> List.tabulate(y)(_ + 1).sum)

    maxHeights
      .sortBy(_._2)
      .reverse
      .foldLeft[Option[Int]](None)((result, velocityAndHeight) =>
        if (result.isDefined) result
        else
          val (yVelocity, maxHeight) =
            (velocityAndHeight._1, velocityAndHeight._2)
          findXVelocity(yVelocity = yVelocity).map(_ => maxHeight)
      )
