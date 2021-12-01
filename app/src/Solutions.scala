object Solutions:
  def sonarScan(in: List[Int], windowSize: Int): Int =
    in.sliding(windowSize)
      .map(_.sum)
      .sliding(2)
      .map { pair => if (pair(1) > pair(0)) 1 else 0 }
      .sum
