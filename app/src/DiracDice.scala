import cats.conversions.all
object DiracDice:

  def addTuple(l: (Long, Long), r: (Long, Long)): (Long, Long) =
    (l._1 + r._1) -> (l._2 + r._2)

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

  // play one universe of games up to a winner
  @scala.annotation.tailrec
  final def playUntilWinner(state: GameState): GameState =
    if (state.winner.nonEmpty)
      state
    else
      playUntilWinner(state.playTurn)

  // find all possible outcomes
  // just keeps the winning player counts to avoid it going too mad
  final def playAllGames(state: GameState): (Int, Int) =
    playAllGames(List(state), (0 -> 0))

  @scala.annotation.tailrec
  private def playAllGames(
      ongoingGames: List[GameState],
      currentScore: (Int, Int)
  ): (Int, Int) =
    def add(l: (Int, Int), r: (Int, Int)): (Int, Int) =
      (l._1 + r._1) -> (l._2 + r._2)

    ongoingGames.headOption match
      case Some(game) =>
        if (game.player1.isWinner)
          playAllGames(ongoingGames.tail, add(currentScore, (1 -> 0)))
        else if (game.player2.isWinner)
          playAllGames(ongoingGames.tail, add(currentScore, (0 -> 1)))
        else
          playAllGames(
            game.universeOfNextSteps ++ ongoingGames.tail,
            currentScore
          )
      case None =>
        currentScore

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
      universeOfNextSteps.apply((Math.random * universeOfNextSteps.size).toInt)

    final lazy val universeOfNextSteps: List[GameState] =
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

// approach the problem looking at the distribution rather than modelling each full game
final case class DiracDice(targetValue: Int):

  type StepDistribution = Map[Int, Long]

  def distribution(startValue: Int): StepDistribution =
    distribution(
      List(
        DiracDice.Player(
          playerNumber = 1,
          score = 0,
          currentSpace = startValue,
          targetScore = targetValue
        ) -> 1
      ),
      currentDepth = 0,
      result = Map.empty
    )

  val possibleDiceRolls: List[Int] = List(1, 2, 3).flatMap { r1 =>
    List(1, 2, 3).flatMap { r2 =>
      List(1, 2, 3).map { r3 =>
        r1 + r2 + r3
      }
    }
  }

  @scala.annotation.tailrec
  private def distribution(
      nodes: List[(DiracDice.Player, Int)],
      currentDepth: Int,
      result: StepDistribution
  ): StepDistribution =
    // println(currentDepth.toString + " " + nodes.map(p => p._1.currentSpace -> p._1.score -> p._2))

    val (remainingNodes, nodesThatReachedTarget) =
      nodes.foldLeft[(List[(DiracDice.Player, Int)], Long)](Nil -> 0) {
        case (searchResult, nextNode) =>
          val (stillToSearch, completedThisStep) = searchResult
          if (nextNode._1.score >= targetValue)
            stillToSearch -> (completedThisStep + nextNode._2)
          else
            (nextNode +: stillToSearch) -> completedThisStep
      }

    val newResult: StepDistribution =
      result + (currentDepth -> nodesThatReachedTarget)

    if (remainingNodes.isEmpty)
      newResult
    else
      distribution(
        remainingNodes.flatMap(nodeValue =>
          possibleDiceRolls
            .map(roll => roll -> nodeValue._1.move(spaces = roll))
            .groupBy(_._1)
            .map { case (roll, results) =>
              results.map(_._2).head -> results.size * nodeValue._2
            }
        ),
        currentDepth + 1,
        newResult
      )

  def determineWinners(
      player1Distribution: StepDistribution,
      player2Distribution: StepDistribution
  ): (Long, Long) =
    // player 1 always goes first
    // if player 1 finishes in n steps
    //  then player 1 wins whenever player 2 finishes in >= n steps, and loses otherwise

    // say player 1 finishes in n steps t_1(n) times
    // player 1 loses if player 2 finishes in at most n - 1 steps, which happens t_2(n -1) + t_2(n - 2) + ... t_2(1) + t_2(0) times

    val allResults = player1Distribution.toList.map {
      case (player1StepCount, player1Frequency) =>
        val (p1Wins, p1Losses) =
          player2Distribution.partition(_._1 >= player1StepCount)
        val result =
          ((player1Frequency * p1Wins.values.sum) -> (player1Frequency * p1Losses.values.sum))
        result
    }
    allResults.toList.fold(0L -> 0L)(DiracDice.addTuple)
