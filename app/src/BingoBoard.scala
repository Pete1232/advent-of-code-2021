import java.util.UUID
case class BingoBoard(
    name: String,
    board: List[List[(Int, Boolean)]]
):

  final def call(number: Int): BingoBoard =
    BingoBoard(
      name,
      board.map { row =>
        row.map { col =>
          if (col._1 == number)
            (col._1, true)
          else
            col
        }
      }
    )

  final val isWinner =
    def isRowWin(rows: List[List[(Int, Boolean)]]): Boolean =
      rows
        .map { row =>
          row.map(_._2).fold(true)(_ && _)
        }
        .fold(false)(_ || _)
    isRowWin(board) || isRowWin(board.transpose)

object BingoBoard:

  def apply(name: String, rows: List[Int]*): BingoBoard =
    BingoBoard.apply(
      name,
      rows.map(_.map((_, false))).toList
    )

  def playRound(thisCall: Int, gameBoards: List[BingoBoard]): List[BingoBoard] =
    gameBoards.map(
      _.call(number = thisCall)
    )

  @scala.annotation.tailrec
  def playRound(
      callSequence: List[Int],
      gameBoards: List[BingoBoard]
  ): Option[(BingoBoard, Int)] =
    callSequence.headOption match
      case Some(call) =>
        val boardsAfterCall = gameBoards.map(
          _.call(number = call) // doesn't handle no winner
        )
        boardsAfterCall.find(_.isWinner) match
          case None =>
            playRound(callSequence.tail, boardsAfterCall)
          case Some(winner) =>
            Some((winner, finalScore(lastCall = call, finalBoard = winner)))
      case None => None

  def finalScore(lastCall: Int, finalBoard: BingoBoard): Int =
    val boardScore = finalBoard.board.flatMap(_.filterNot(_._2)).map(_._1).sum
    boardScore * lastCall

  @scala.annotation.tailrec
  def parseCardsFromText(
      input: List[String],
      output: List[BingoBoard] = Nil,
      currentCardBeingBuilt: List[List[Int]] = Nil
  ): List[BingoBoard] =
    val thisRow = input.headOption
    if (thisRow.isEmpty) output
    else if (thisRow.head.isEmpty)
      parseCardsFromText(
        input = input.tail,
        output = output :+ BingoBoard(
          (output.size + 1).toString,
          currentCardBeingBuilt: _*
        ),
        currentCardBeingBuilt = Nil
      )
    else
      val parsed =
        thisRow.head.split(" ").filterNot(_ == " ").filterNot(_.isEmpty).toList.map(_.toInt)
      parseCardsFromText(
        input = input.tail,
        output = output,
        currentCardBeingBuilt = currentCardBeingBuilt :+ parsed
      )
