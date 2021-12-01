object Solutions:
  def sonarScan(in: List[Int]): Int =
    in.sliding(2).map { pair => if (pair(1) > pair(0)) 1 else 0 }.sum
