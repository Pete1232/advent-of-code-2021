// object SevenSegmentSearch:
//   val properWiring: Map[Int, List[String]] = Map(
//     1 -> List("c", "f"),
//     7 -> List("a", "c", "f"),
//     4 -> List("b", "c", "d", "f"),
//     2 -> List("a", "c", "d", "e", "g"),
//     3 -> List("a", "c", "d", "f", "g"),
//     5 -> List("a", "b", "d", "f", "g"),
//     0 -> List("a", "b", "c", "e", "f", "g"),
//     6 -> List("a", "b", "d", "e", "f", "g"),
//     9 -> List("a", "b", "c", "d", "f", "g"),
//     8 -> List("a", "b", "c", "d", "e", "f", "g")
//   )

case class SevenSegmentDisplay(patterns: List[String]):
  // first count unique display sizes
  val display1: String = patterns.find(_.size == 2).get
  val display7: String = patterns.find(_.size == 3).get
  val display4: String = patterns.find(_.size == 4).get
  val display8: String = patterns.find(_.size == 7).get

  val aSegment: Char = display7.diff(display1).charAt(0)

  val allCharacters: String = patterns.fold("")(_ + _)

  // e appears 4 times
  // b appears 6 times
  // d appears 7 times
  // g appears 7 times
  // a appears 8 times
  // c appears 8 times
  // f appears 9 times

  val countOfEachCharacter: Map[Char, Int] =
    allCharacters.groupMapReduce(identity)(_ => 1)(_ + _)

  def findWithCount(count: Int): Char =
    countOfEachCharacter.find((k, v) => v == count).get._1

  val bSegment: Char = findWithCount(6)
  val eSegment: Char = findWithCount(4)
  val fSegment: Char = findWithCount(9)

  // can easily get the c segment now
  val cSegment: Char = display1.toList.filterNot(_ == fSegment).head

  // now know which characters correspond to a, b, c, e, f - so can find g
  val allOfABCEF: List[Char] =
    List(aSegment, bSegment, cSegment, eSegment, fSegment)
  val displaysWith6Segments: List[String] = patterns.filter(_.size == 6)
  val display0: String =
    displaysWith6Segments.find(_.toList.diff(allOfABCEF).size == 1).get
  val gSegment: Char = display0.diff(allOfABCEF).head

  // now have a, b, c, e, f, g - can find d
  val allOfABCEFG: List[Char] = allOfABCEF :+ gSegment
  val dSegment: Char = display8.filterNot(allOfABCEFG.contains).head

  // now displays. We need to be able to go from a list of characters to a number (the displayed digit)

  val wiring = Map(
    List(aSegment, bSegment, cSegment, eSegment, fSegment, gSegment) -> 0,
    List(cSegment, fSegment) -> 1,
    List(aSegment, cSegment, dSegment, eSegment, gSegment) -> 2,
    List(aSegment, cSegment, dSegment, fSegment, gSegment) -> 3,
    List(bSegment, cSegment, dSegment, fSegment) -> 4,
    List(aSegment, bSegment, dSegment, fSegment, gSegment) -> 5,
    List(aSegment, bSegment, dSegment, eSegment, fSegment, gSegment) -> 6,
    List(aSegment, cSegment, fSegment) -> 7,
    List(
      aSegment,
      bSegment,
      cSegment,
      dSegment,
      eSegment,
      fSegment,
      gSegment
    ) -> 8,
    List(aSegment, bSegment, cSegment, dSegment, fSegment, gSegment) -> 9
  )

  def toDigit(input: Char*): Int =
    wiring.find((characters, digit) =>
      characters.sorted.equals(input.sorted)
    ).get._2

  def toString(on: Char*) =
    val off = '.'
    val a = if (on.contains(aSegment)) aSegment else off
    val b = if (on.contains(bSegment)) bSegment else off
    val c = if (on.contains(cSegment)) cSegment else off
    val d = if (on.contains(dSegment)) dSegment else off
    val e = if (on.contains(eSegment)) eSegment else off
    val f = if (on.contains(fSegment)) fSegment else off
    val g = if (on.contains(gSegment)) gSegment else off
    s"""
     $a$a$a$a
    $b    $c
    $b    $c
     $d$d$d$d
    $e    $f
    $e    $f
     $g$g$g$g
    """

  override def toString() =
    toString("abcdefg".toCharArray: _*)
