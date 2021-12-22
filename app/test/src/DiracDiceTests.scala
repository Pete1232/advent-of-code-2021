import utest._
import Dice.DeterministicD100

object DiracDiceTests extends TestSuite:

  val tests = Tests {
    test("the deterministic die should work") - {
      val die: DeterministicD100 = Dice.DeterministicD100(currentValue = 1)
      assert(die.roll(1)._1.currentValue == 2)
      assert(die.roll(9)._1.currentValue == 10)
      assert(die.roll(5)._1.roll(9)._1.currentValue == 15)
      assert(die.roll(99)._1.currentValue == 100)
      assert(die.roll(100)._1.currentValue == 1)
      assert(die.roll(200)._1.currentValue == 1)

      val total1 = die.roll(99)._2
      assert(total1 == 5049)

      val total2 = die.roll(100)._2
      assert(total2 == 5050)

      val total3 = die.roll(199)._2
      assert(total3 == 10099)

      val total4 = die.roll(200)._2
      assert(total4 == 10100)

      val dieStart = Dice.DeterministicD100(currentValue = 0)
      assert(dieStart.roll(1)._1.currentValue == 1)
    }

    test("test rounds") - {
      val gameStartState = DiracDice.newGame(
        p1Start = 4,
        p2Start = 8,
        die = Dice.DeterministicD100(currentValue = 0),
        targetScore = 1000
      )

      val oneTurnEach = gameStartState.playTurn.playTurn

      assert(oneTurnEach.player1.currentSpace == 10)
      assert(oneTurnEach.player1.score == 10)
      assert(oneTurnEach.player2.currentSpace == 3)
      assert(oneTurnEach.player2.score == 3)

      val fourTurnsEach =
        oneTurnEach.playTurn.playTurn.playTurn.playTurn.playTurn.playTurn

      assert(fourTurnsEach.player1.currentSpace == 6)
      assert(fourTurnsEach.player1.score == 26)
      assert(fourTurnsEach.player2.currentSpace == 6)
      assert(fourTurnsEach.player2.score == 22)
    }

    test("test game") - {
      val gameStartState = DiracDice.newGame(
        p1Start = 4,
        p2Start = 8,
        die = Dice.DeterministicD100(currentValue = 0),
        targetScore = 1000
      )

      val result = DiracDice.playUntilWinner(gameStartState)

      assert(result.winner.map(_.playerNumber) == Some(1))
      assert(result.answer == Some(739785))
    }

    test("answer") - {
      val gameStartState = DiracDice.newGame(
        p1Start = 10,
        p2Start = 3,
        die = Dice.DeterministicD100(currentValue = 0),
        targetScore = 1000
      )

      val result = DiracDice.playUntilWinner(gameStartState)

      assert(result.answer == Some(742257))
    }

    test("find the distribution") - {
      val result = DiracDice(targetValue = 4).distribution(startValue = 0)
      assert(result == Map(0 -> 0, 1 -> 0, 2 -> 6, 3 -> 8, 4 -> 3))

      val result2 = DiracDice(targetValue = 21).distribution(startValue = 0)
      assert(result2.size == 22)
      assert(result2.maxBy(_._1) == 21 -> 3)
    }

    test("all games example") - {
      val gameStartState = DiracDice.newGame(
        p1Start = 4,
        p2Start = 8,
        die = Dice.D3,
        targetScore = 5
      )

      val universes = DiracDice.playAllGames(gameStartState)

      println(universes)
    }
  }
