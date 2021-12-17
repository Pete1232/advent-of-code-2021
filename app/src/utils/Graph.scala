package utils

object Graph:

  def search(vertex: Int, adjacencyMatrix: Grid): List[List[Int]] =
    val adjacent = adjacencyMatrix.underlying
      .filter(entry => entry._1._1 == vertex && entry._2 >= 1)
      .keySet
      .map(_._1)
      .toList
    adjacent.flatMap { nextVertex =>
      if (nextVertex == 1)
        List(nextVertex +: List(1))
      else
        search(
          nextVertex,
          Grid(
            adjacencyMatrix.underlying.map((entry, connections) =>
              if (entry._1 == vertex)
                entry -> (connections - 1)
              else
                entry -> connections
            )
          )
        ).map(nextVertex +: _)
    }
