import utest._

object TrickShotTests extends TestSuite:
  val tests = Tests {

    val target = Target(20, 30, -10, -5)

    test("(7,2) should hit the target") - {
      val result = target.trickShot(
        start = (0, 0),
        xVelocity = 7,
        yVelocity = 2
      )
      assert(result == Some(28 -> -7))

      // (6,2) also hits, so this actually correctly returns 6
      val calculatedXVelocity = target.findXVelocity(yVelocity = 2)
      assert(calculatedXVelocity == Some(6))
    }

    test("(6,3) should hit the target") - {
      val result = target.trickShot(
        start = (0, 0),
        xVelocity = 6,
        yVelocity = 3
      )
      assert(result == Some(21 -> -9))

      val calculatedXVelocity = target.findXVelocity(yVelocity = 3)
      assert(calculatedXVelocity == Some(6))
    }

    test("(17,-4) should NOT hit the target") - {
      val result = target.trickShot(
        start = (0, 0),
        xVelocity = 17,
        yVelocity = -4
      )
      assert(result == None)

      // a velocity of 11 will hit at (21,-9) before flying off into the distance
      val calculatedXVelocity = target.findXVelocity(yVelocity = -4)
      assert(calculatedXVelocity == Some(11))
    }

    test("find the maximum height that will hit the target") - {
      val result = target.bestShot
      assert(result == Some(45))
    }

    test("answer 1") - {
      val puzzleTarget = Target(192, 251, -89, -59)
      assert(puzzleTarget.bestShot == Some(3916))
    }
  }
