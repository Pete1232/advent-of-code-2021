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

    // done some on paper
    test("distribution test") - {
      val game = DiracDice(targetValue = 5)
      val player1Distribution = game.distribution(startValue = 4)
      assert(player1Distribution.get(1) == Some(17))
      assert(player1Distribution.get(2) == Some(27 * 6 + 26 * 3 + 24 * 1))
      assert(player1Distribution.get(3) == Some(162))
    }

    test("distribution test with a different movement rule") - {
      // this game just roll and add to the score each time
      val game1 = DiracDice(
        targetValue = 7,
        movementRule = (player, roll) =>
          player.copy(
            currentSpace = player.currentSpace + roll,
            score = player.score + roll
          )
      )
      val player1Distribution = game1.distribution(
        startValue = 0,
        possibleDiceRolls = List(3, 4, 4, 4, 5, 5, 5, 6)
      )
      assert(player1Distribution == Map(0 -> 0, 1 -> 0, 2 -> 63, 3 -> 8))

      val game2 = DiracDice(
        targetValue = 7,
        movementRule = (player, roll) =>
          player.copy(
            currentSpace = player.currentSpace + roll,
            score =
              player.score + roll + 1 // +1 to account for the start position with this rule
          )
      )

      val player2Distribution = game2.distribution(
        startValue = 1,
        possibleDiceRolls = List(3, 4, 4, 4, 5, 5, 5, 6)
      )
      assert(player2Distribution == Map(0 -> 0, 1 -> 1, 2 -> 56))
    }

    test("all games example") - {
      val game = DiracDice(targetValue = 21)
      val player1Distribution = game.distribution(startValue = 4)
      val player2Distribution = game.distribution(startValue = 8)
      val result =
        game.determineWinners(player1Distribution, player2Distribution)

      println(result)
    }
  }
