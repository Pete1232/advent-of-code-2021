import java.util.function.IntBinaryOperator
import scala.collection.BitSet
object Solutions:

  def sonarScan(in: List[Int], windowSize: Int): Int =
    in.sliding(windowSize)
      .map(_.sum)
      .sliding(2)
      .map { pair => if (pair(1) > pair(0)) 1 else 0 }
      .sum

  def dive(instructions: List[(String, Int)]) =
    instructions.foldLeft((0, (0, 0))) {
      (nextPosition: (Int, (Int, Int)), instruction: (String, Int)) =>
        nextDive(
          instruction._1,
          instruction._2,
          nextPosition._1,
          nextPosition._2
        )
    }

  // doesn't handle unknown verbs nicely
  // doesn't handle going "up" more than is sensible
  private def nextDive(
      verb: String,
      magnitude: Int,
      currentAim: Int,
      currentPosition: (Int, Int)
  ): (Int, (Int, Int)) = // aim, (forward, depth)
    verb match
      case "forward" =>
        (
          currentAim,
          (
            currentPosition._1 + magnitude,
            currentPosition._2 + (magnitude * currentAim)
          )
        )
      case "up" =>
        (currentAim - magnitude, (currentPosition._1, currentPosition._2))
      case "down" =>
        (currentAim + magnitude, (currentPosition._1, currentPosition._2))

  // report used to generate a gamma rate and epsilon rate, all binary
  // power consumption = gamma * epsilon
  // the bit in position X of gamma is the most common bit in position X for the whole report
  // epsilon rate is the lest common
  // power = gamma * epsilon
  def diagnosticPower(in: List[List[Int]]): Int =
    val reportSize = in.size
    val totalCount = in.fold(Nil) { (l, r) =>
      // using zip assumes equal length, zipAll may work for differing lengths?
      l.zipAll(r, 0, 0).map(_ + _)
    }

    val gamma = totalCount.map { columnCount =>
      if (columnCount > reportSize / 2)
        1
      else
        0
    }

    val epsilon = gamma.map(bit => Math.abs(bit - 1))

    binaryToDecimal(gamma) * binaryToDecimal(epsilon)

  // life support = o2 * co2
  // take first bit of the whole report
  // apply criteria to find a sub-list from the report
  // repeat, moving to the right until the result is found
  def oxygenReport(input: List[List[Int]]) =

    @scala.annotation.tailrec
    def position(in: List[List[Int]], pos: Int): List[Int] =
      val reportSize = in.size
      val totalCount = in.fold(Nil) { (l, r) =>
        l.zipAll(r, 0, 0).map(_ + _)
      }
      val gamma = totalCount.map { columnCount =>
        if (columnCount >= reportSize - columnCount)
          1
        else
          0
      }
      val remaining = in.filter(_.apply(pos) == gamma.apply(pos))
      if (remaining.size == 1)
        remaining.head
      else
        position(remaining, pos + 1)

    binaryToDecimal(position(input, 0))

  def carbonDioxideReport(input: List[List[Int]]) =

    @scala.annotation.tailrec
    def position(in: List[List[Int]], pos: Int): List[Int] =
      val reportSize = in.size
      val totalCount = in.fold(Nil) { (l, r) =>
        l.zipAll(r, 0, 0).map(_ + _)
      }
      val gamma = totalCount.map { columnCount =>
        if (columnCount >= reportSize - columnCount)
          1
        else
          0
      }
      val epsilon = gamma.map(bit => Math.abs(bit - 1))
      val remaining = in.filter(_.apply(pos) == epsilon.apply(pos))
      if (remaining.size == 1)
        remaining.head
      else
        position(remaining, pos + 1)

    binaryToDecimal(position(input, 0))

  def binaryToDecimal(binary: List[Int]): Int =
    binary.reverse.zipWithIndex
      .map {
        case (bit, index) if bit == 1 => Math.pow(2, index)
        case _                        => 0
      }
      .sum
      .toInt
