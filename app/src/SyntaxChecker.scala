import scala.collection.immutable.Queue
import cats.data.EitherT
object SyntaxChecker:

  final private val bracketPairs = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  def lintAll(s: String): Either[String, Int] =
    EitherT(s.split("\n").toList.map(lintLine))
      .foldLeft[Either[String, Int]](Right(0))((total, lineResult) =>
        total.map(_ + lineResult)
      )

  def lintLine(s: String): Either[String, Int] = lintLine(s, List.empty)

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
  ): Either[String, Int] =
    s.headOption match
      case None | Some('\n') =>
        // end of the line
        Right(0)
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

  private def score(c: Char): Either[String, Int] =
    c match
      case ')' => Right(3)
      case ']' => Right(57)
      case '}' => Right(1197)
      case '>' => Right(25137)
      case _   => Left(s"$c is not a valid close")
