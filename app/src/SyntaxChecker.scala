import scala.collection.immutable.Queue
import cats.data.EitherT

object SyntaxChecker:

  final private val bracketPairs = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  enum LineType:
    case Corrupt, Incomplete, Valid

  def lintAll(s: String, errorType: LineType): Either[String, Int] =
    EitherT(
      s.split("\n")
        .toList
        .map(lintLine)
        .filter(_.map(_._2 == errorType).getOrElse(false))
    )
      .foldLeft[Either[String, Int]](Right(0))((total, lineResult) =>
        total.map(_ + lineResult._1)
      )

  def lintLine(s: String): Either[String, (Int, LineType)] =
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
  ): Either[String, (Int, LineType)] =
    s.headOption match
      case None | Some('\n') =>
        // end of the line
        if (currentlyOpen.isEmpty)
          Right(0, LineType.Valid)
        else
          Right(0, LineType.Incomplete)
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
              score(c)
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
                  score(c)

  private def score(c: Char): Either[String, (Int, LineType)] =
    Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
      .get(c)
      .toRight(s"$c is not a valid close")
      .map(_ -> LineType.Corrupt)
