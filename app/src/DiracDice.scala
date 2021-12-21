object DiracDice:

  def newGame(
      p1Start: Int,
      p2Start: Int,
      die: Dice,
      targetScore: Int
  ): GameState = GameState(
    Player(playerNumber = 1, score = 0, currentSpace = p1Start, targetScore),
    Player(playerNumber = 2, score = 0, currentSpace = p2Start, targetScore),
    die = die,
    isPlayer1Turn = true,
    rolls = 0
  )

  @scala.annotation.tailrec
  final def playUntilWinner(state: GameState): GameState =
    if (state.winner.nonEmpty)
      state
    else
      playUntilWinner(state.playTurn)

  case class Player(
      playerNumber: Int,
      score: Int,
      currentSpace: Int,
      targetScore: Int
  ):
    final val isWinner = score >= targetScore

    def move(spaces: Int): Player =
      // slightly complicated formula to offset by 1 (for 1-based counting)
      val newSpace = ((currentSpace + spaces + 9) % 10) + 1
      this.copy(score = score + newSpace, currentSpace = newSpace)

  case class GameState(
      player1: Player,
      player2: Player,
      die: Dice,
      isPlayer1Turn: Boolean,
      rolls: Int
  ):

    final lazy val (winner, loser) =
      if (player1.isWinner)
        Some(player1) -> Some(player2)
      else if (player2.isWinner)
        Some(player2) -> Some(player1)
      else
        None -> None

    final lazy val answer = loser.map(_.score * rolls)

    final lazy val playTurn: GameState =
      val possibleNextSteps = universeOfNextSteps(die)
      possibleNextSteps.apply((Math.random * possibleNextSteps.size).toInt)

    final def universeOfNextSteps(die: Dice): List[GameState] =
      val possibleRolls = die.roll(1)._1.possibleValues.flatMap { roll1 =>
        die.roll(2)._1.possibleValues.flatMap { roll2 =>
          die.roll(3)._1.possibleValues.map { roll3 =>
            (roll1, roll2, roll3)
          }
        }
      }
      val rolledDie = die.roll(3)._1

      possibleRolls.map { case (r1, r2, r3) =>
        val total = r1 + r2 + r3
        if (isPlayer1Turn)
          this.copy(
            player1 = player1.move(total),
            die = rolledDie,
            isPlayer1Turn = false,
            rolls = rolls + 3
          )
        else
          this.copy(
            player2 = player2.move(total),
            die = rolledDie,
            isPlayer1Turn = true,
            rolls = rolls + 3
          )
      }

sealed trait Dice:
  def possibleValues: List[Int]
  def roll(times: Int): (Dice, Int)

object Dice:

  object D3 extends Dice:
    def roll(times: Int): (D3.type, Int) =
      (D3 -> (Math.random * 3).toInt)

    lazy val possibleValues: List[Int] = List(1, 2, 3)

  case class DeterministicD100(currentValue: Int) extends Dice:

    def roll(times: Int): (DeterministicD100, Int) =
      roll(times, currentValue, 0)

    lazy val possibleValues: List[Int] = List(currentValue)

    @scala.annotation.tailrec
    private def roll(
        times: Int,
        value: Int,
        total: Int
    ): (DeterministicD100, Int) =
      val newValue =
        val v = ((value + 1) % 101)
        if (v == 0) 1 else v
      if (times <= 1)
        DeterministicD100(newValue) -> (total + newValue)
      else
        roll(times - 1, newValue, total + newValue)
