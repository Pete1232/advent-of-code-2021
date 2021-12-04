import Solutions._
import utest._

object BingoBoardTests extends TestSuite:
  val tests = Tests {

    val callSequence = List(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16,
      13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1)

    val board1 = BingoBoard(
      "1",
      List(22, 13, 17, 11, 0),
      List(8, 2, 23, 4, 24),
      List(21, 9, 14, 16, 7),
      List(6, 10, 3, 18, 5),
      List(1, 12, 20, 15, 19)
    )

    val board2 = BingoBoard(
      "2",
      List(3, 15, 0, 2, 22),
      List(9, 18, 13, 17, 5),
      List(19, 8, 7, 25, 23),
      List(20, 11, 10, 24, 4),
      List(14, 21, 16, 12, 6)
    )

    val board3 = BingoBoard(
      "3",
      List(14, 21, 17, 24, 4),
      List(10, 16, 15, 9, 19),
      List(18, 8, 23, 26, 20),
      List(22, 11, 13, 6, 5),
      List(2, 0, 12, 3, 7)
    )

    test("bingo board") - {
      test("return None if no winner") - {
        val result = BingoBoard.playRound(
          callSequence = Nil,
          gameBoards = List(board1)
        )
        assert(result == None)
      }
      test("return no winner for an empty board") - {
        assert(board1.isWinner == false)
        assert(board2.isWinner == false)
        assert(board3.isWinner == false)
      }
      test("first 5") - {
        val first5 = callSequence.take(5)
        val result = BingoBoard.playRound(
          callSequence = first5,
          gameBoards = List(board1, board2, board3)
        )
        assert(result == None)
      }
      test("first 11") - {
        val first11 = callSequence.take(11)
        val result = BingoBoard.playRound(
          callSequence = first11,
          gameBoards = List(board1, board2, board3)
        )
        assert(result == None)
      }
      test("first 12") - {
        val first12 = callSequence.take(12)
        val result = BingoBoard.playRound(
          callSequence = first12,
          gameBoards = List(board1, board2, board3)
        )
        assert(result.map(_._1.name) == Some(board3.name))
        assert(result.map(_._2) == Some(4512))
      }
      test("full sequence") - {
        val calls = callSequence
        val result = BingoBoard.playRound(
          callSequence = calls,
          gameBoards = List(board1, board2, board3)
        )
        assert(result.map(_._1.name) == Some(board3.name))
        assert(result.map(_._2) == Some(4512))
      }
      test("parse from texr") - {
        val input = List(
          " 6 71 87 35 74",
          "40 16 19 73 69",
          " 1 67 42 78 23",
          "49 59 65 45 53",
          "48 82 30 72 39",
          "",
          "39 31 13  2 38",
          "60 65 18  7  1",
          "74 23 78 51  4",
          "50 61 83 94 25",
          "34  3 80  6 87",
          "",
          "87 15 42 55 64",
          "93 30 83 80 46",
          "24 81 26 31  8",
          "84 14 67 82 23",
          "75 22 94 74 40",
          "",
          "40 21 75  2 78",
          "25 15 49 61 55",
          "98 70 92 93 63",
          "53  1  0 33 32",
          "12 59 18 44 73",
          "",
          "78 11 12 58 61",
          "26  8 51 28 69",
          "64 35 89 95  1",
          "20 79 62 13 83",
          "53  7 84 18 34",
          ""
        )
        val cards = BingoBoard.parseCardsFromText(input)
        assert(cards.head.board.head.map(_._1) == List(6, 71, 87, 35, 74))
        assert(cards.apply(4).board.head.map(_._1) == List(78, 11, 12, 58, 61))
        assert(cards.apply(4).board.last.map(_._1) == List(53, 7, 84, 18, 34))
      }
    }
  }
