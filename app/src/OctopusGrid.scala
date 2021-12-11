package utils

import cats.Show

case class OctopusGrid(
    underlying: Map[(Int, Int), Int],
    private val flashesUntilNow: Int
):

  lazy val nextStep: OctopusGrid =
    OctopusGrid(nextStep(underlying.mapValues(_ + 1).toMap), totalFlashes)

  // todo - redundant to iterate through again
  lazy val flashes: Int = underlying.count(_._2 == 0)

  lazy val totalFlashes: Int = flashesUntilNow + flashes

  lazy val allFlashed: Boolean = underlying.keySet.size == flashes

  def stepUntilAllFlashed(
      maxSteps: Int,
      steps: Int = 0
  ): Option[(OctopusGrid, Int)] =
    if (steps == maxSteps || allFlashed)
      Some(this -> steps)
    else if (steps == maxSteps)
      None
    else
      nextStep.stepUntilAllFlashed(maxSteps, steps + 1)

  def step(count: Int): OctopusGrid =
    if (count == 0)
      this
    else
      nextStep.step(count - 1)

  private def nextStep(grid: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
    val result =
      grid.foldLeft(grid)((currentState, pointAndOldValue) =>
        val currentPointValue = currentState(pointAndOldValue._1)
        if (currentPointValue >= 10)
          flash(pointAndOldValue._1, currentState)
        else
          currentState
      )
    if (result.eq(grid)) // intentionally checking reference equality
      result
    else
      nextStep(result)

  private def flash(
      point: (Int, Int),
      state: Map[(Int, Int), Int]
  ): Map[(Int, Int), Int] =
    val upLeft = (point._1 - 1, point._2 - 1)
    val upMiddle = (point._1, point._2 - 1)
    val upRight = (point._1 + 1, point._2 - 1)
    val middleLeft = (point._1 - 1, point._2)
    val middleRight = (point._1 + 1, point._2)
    val downLeft = (point._1 - 1, point._2 + 1)
    val downMiddle = (point._1, point._2 + 1)
    val downRight = (point._1 + 1, point._2 + 1)

    def inc(i: Int) =
      if (i == 0) 0 else i + 1

    state
      .updatedWith(upLeft)(_.map(inc))
      .updatedWith(upMiddle)(_.map(inc))
      .updatedWith(upRight)(_.map(inc))
      .updatedWith(middleLeft)(_.map(inc))
      .updatedWith(middleRight)(_.map(inc))
      .updatedWith(downLeft)(_.map(inc))
      .updatedWith(downMiddle)(_.map(inc))
      .updatedWith(downRight)(_.map(inc))
      .updatedWith(point)(_.map(_ => 0))

object OctopusGrid:
  def buildGrid(s: String): OctopusGrid =
    OctopusGrid(buildGrid(s, 0, 0, Map.empty), 0)

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

  implicit val showGrid: Show[OctopusGrid] = Show.show(map =>
    val maxX = map.underlying.maxBy(_._1._1)._1._1
    val maxY = map.underlying.maxBy(_._1._2)._1._2
    val grid =
      List.tabulate(maxY + 1, maxX + 1)((y, x) => map.underlying((x, y)))
    grid.map(_.mkString(" ")).mkString("\n")
  )
