import scala.collection.immutable.Queue
import cats.data.EitherT
import cats.Eval

object SyntaxChecker:

  final private val bracketPairs = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  enum LineType:
    case Corrupt, Incomplete, Valid

  def lintAll(s: String): Either[String, Long] =
    EitherT(
      s.split("\n")
        .toList
        .map(lintLine)
        .filter(_.map(_._2 == LineType.Corrupt).getOrElse(false))
    )
      .foldLeft[Either[String, Long]](Right(0))((total, lineResult) =>
        total.map(_ + lineResult._1)
      )

  def completeAll(s: String): Either[String, Long] =
    EitherT(
      s.split("\n")
        .toList
        .map(lintLine)
        .filter(_.map(_._2 == LineType.Incomplete).getOrElse(false))
    )
      .foldLeft[Either[String, (List[Long], Int)]](Right(List.empty -> 0))(
        (total, lineResult) =>
          total.map(t => (lineResult._1 +: t._1) -> (t._2 + 1))
      )
      .flatMap(scores =>
        scores._1.sorted
          .drop(scores._2 / 2)
          .headOption
          .toRight(s"Error trying to find the midpoint of ${scores._1} ")
      )

  def lintLine(s: String): Either[String, (Long, LineType)] =
    lintLine(s, List.empty)

  /** Lint a line to see if the churn syntax is correct
    * @return
    *   the score of the error for this line
    * @return
    *   a description of the error on the left, if the function errors for any
    *   reason
    */
  @scala.annotation.tailrec
  private def lintLine(
      s: String,
      currentlyOpen: List[Char]
  ): Either[String, (Long, LineType)] =
    s.headOption match
      case None | Some('\n') =>
        // end of the line
        if (currentlyOpen.isEmpty)
          Right(0, LineType.Valid)
        else
          scoreCompletion(stillOpen = currentlyOpen)
      case Some(c) =>
        // somewhere in the line
        // note if the currentlyOpen list is empty at this point we're looking at a new chunk
        if (bracketPairs.keySet.contains(c))
          // if its an opening character it must still be valid, we expect this to close at some point
          lintLine(s.tail, c +: currentlyOpen)
        else
          // since this isn't an opening character it must be a closing
          // which one is it? It needs to match the most recently opened to be valid
          currentlyOpen.headOption match
            case None =>
              // error case, since we can't close what isn't open
              scoreCorrupt(c)
            case Some(open) =>
              // for this to be valid it has to match the most recently opened chunk
              bracketPairs.get(open) match
                case None =>
                  Left(s"$open is not a valid open")
                case Some(expected) if c == expected =>
                  // if it matches close off the chunk
                  lintLine(s.tail, currentlyOpen.tail)
                case _ =>
                  // otherwise score up
                  scoreCorrupt(c)

  private def scoreCorrupt(c: Char): Either[String, (Long, LineType)] =
    Map(')' -> 3L, ']' -> 57L, '}' -> 1197L, '>' -> 25137L)
      .get(c)
      .toRight(s"$c is not a valid close")
      .map(_ -> LineType.Corrupt)

  private def scoreCompletion(
      stillOpen: List[Char]
  ): Either[String, (Long, LineType)] =

    val closures = EitherT(
      stillOpen
        .map(c => bracketPairs.get(c).toRight(s"$c is not a valid open"))
    )

    val scores: EitherT[List, String, Long] = closures.subflatMap { c =>
      Map(')' -> 1L, ']' -> 2L, '}' -> 3L, '>' -> 4L)
        .get(c)
        .toRight(s"$c is not a valid close")
    }

    scores
      .foldLeft[Either[String, Long]](Right(0))((total, score) =>
        total.map(_ * 5 + score)
      )
      .map(_ -> LineType.Incomplete)
