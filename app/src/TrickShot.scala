object TrickShot:

  def findBestShot(target: Target): Option[Int] =
    val xRange = Range(0, target.maxXVelocity)
    val yRange = Range(target.minYVelocity, target.maxYVelocity)

    val maxHeights = yRange.map(y => y -> List.tabulate(y)(_ + 1).sum)

    @scala.annotation.tailrec
    def findXVelocity(yVelocity: Int, x: Int = 0): Option[Int] =
      if(x < target.xMax)
        step(
          start = 0 -> 0,
          xVelocity = x,
          yVelocity = yVelocity
        ) match {
          case None =>
            findXVelocity(x + 1, yVelocity)
          case _ => Some(x)
        }
      else
        None

    @scala.annotation.tailrec
    def step(start: (Int, Int), xVelocity: Int, yVelocity: Int): Option[(Int, Int)] =
      val nextX = start._1 + xVelocity
      val nextY = start._1 + yVelocity
      if(xRange.contains(nextX) && yRange.contains(nextY))
        Some(nextX -> nextY)
      else if (nextX < target.xMax && nextY > target.yMin && xVelocity != 0)
        step(
          nextX -> nextY,
          xVelocity = Math.max(xVelocity - 1, 0),
          yVelocity = yVelocity - 1
        )
      else
        None
    
    maxHeights.sortBy(_._2).reverse.foldLeft[Option[Int]](None)( (result, velocityAndHeight) =>
      println(velocityAndHeight)
      if(result.isDefined)
        result
      else
        val (yVelocity, maxHeight) = (velocityAndHeight._1, velocityAndHeight._2)
        findXVelocity(yVelocity = yVelocity).map(_ => maxHeight)
    )
  
case class Target(xMin: Int, xMax: Int, yMin: Int, yMax: Int):
  require(xMin < xMax)
  require(yMin < yMax && yMax < 0)

  // more than this will drop below the minimum y position
  val minYVelocity: Int = yMin
  val maxYVelocity: Int = Math.abs(yMin)

  // more than this will overshoot the maximum x position
  val maxXVelocity: Int = xMax
