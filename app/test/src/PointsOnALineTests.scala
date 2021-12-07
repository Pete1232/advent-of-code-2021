import Solutions._
import utest._

object PointsOnALineTests extends TestSuite:
  val tests = Tests {
    val positions = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
    test("calculate the closest position") - {
      val count =
        PointsOnALine.closestPoint(positions)(PointsOnALine.simpleDistance)
      assert(count._1 == 2)
      assert(count._2 == 37)
    }
    test("simple distance to point 1") - {
      val count =
        PointsOnALine.distanceTo(1, positions)(PointsOnALine.simpleDistance)
      assert(count == 41)
    }
    test("simple distance to point 3") - {
      val count =
        PointsOnALine.distanceTo(3, positions)(PointsOnALine.simpleDistance)
      assert(count == 39)
    }
    test("simple distance to point 10") - {
      val count =
        PointsOnALine.distanceTo(10, positions)(PointsOnALine.simpleDistance)
      assert(count == 71)
    }

    test("complicated distance between two points") - {
      assert(PointsOnALine.complicatedDistance(16, 5) == 66)
      assert(PointsOnALine.complicatedDistance(1, 5) == 10)
      assert(PointsOnALine.complicatedDistance(0, 5) == 15)
    }
    test("calculate the complicated closest position") - {
      val count =
        PointsOnALine.closestPoint(positions)(PointsOnALine.complicatedDistance)
      assert(count._1 == 5)
      assert(count._2 == 168)
    }
    test("complicated distance to point 2") - {
      val count = PointsOnALine.distanceTo(2, positions)(
        PointsOnALine.complicatedDistance
      )
      assert(count == 206)
    }
  }
