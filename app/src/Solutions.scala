object Solutions:

  def sonarScan(in: List[Int], windowSize: Int): Int =
    in.sliding(windowSize)
      .map(_.sum)
      .sliding(2)
      .map { pair => if (pair(1) > pair(0)) 1 else 0 }
      .sum

  def dive(instructions: List[(String, Int)]) =
    instructions.foldRight((0, 0)) {
      (instruction: (String, Int), nextPosition: (Int, Int)) =>
        nextDive(instruction._1, instruction._2, nextPosition)
    }

  // doesn't handle unknown verbs nicely
  // doesn't handle going "up" more than is sensible
  private def nextDive(
      verb: String,
      magnitude: Int,
      currentPosition: (Int, Int)
  ): (Int, Int) =
    verb match
      case "forward" =>
        (currentPosition._1 + magnitude, currentPosition._2)
      case "up" =>
        (currentPosition._1, currentPosition._2 - magnitude)
      case "down" =>
        (currentPosition._1, currentPosition._2 + magnitude)
