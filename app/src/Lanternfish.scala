// each produces one more every 7 days
// not synchronised
// model each fish as a number, of days until new fishy
// new lanternfish won't produce another until an extra 2 days pass (so 9)
// 0 based
// "each other number decreases by 1 if it was present at the start of the day"

// count instead of modelling each fish?

object Lanternfish:

  // any fish on 0 at the start of the day will be added to the new 6 count
  // AND for every fish on 0 at the start of the day one will be added to the 8 count
  def nextDay(distribution: Map[Int, Long]): Map[Int, Long] =
    Map(
      0 -> distribution.get(1),
      1 -> distribution.get(2),
      2 -> distribution.get(3),
      3 -> distribution.get(4),
      4 -> distribution.get(5),
      5 -> distribution.get(6),
      6 -> distribution
        .get(7)
        .flatMap(sevens => distribution.get(0).map(zeros => sevens + zeros)),
      7 -> distribution.get(8),
      8 -> distribution.get(0)
    ).mapValues(_.getOrElse(0L)).toMap

  def nextDays(days: Int, initialDistribution: Map[Int, Long]) =
    List
      .fill(days)(0)
      .foldLeft(initialDistribution) { (distribution, _) =>
        Lanternfish.nextDay(distribution)
      }

  def countFish(fish: List[Int]): Map[Int, Long] =
    fish.groupBy(identity).mapValues(_.size.toLong).toMap
