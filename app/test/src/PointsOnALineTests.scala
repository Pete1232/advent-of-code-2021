import Solutions._
import utest._

object PointsOnALineTests extends TestSuite:
  val tests = Tests {
    val positions = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
    test("calculate the closest position") - {
      val count = PointsOnALine.closestPoint(positions)
      assert(count._1 == 2)
      assert(count._2 == 37)
    }
    test("distance to point 1") - {
      val count = PointsOnALine.distanceTo(1, positions)
      assert(count == 41)
    }
    test("distance to point 3") - {
      val count = PointsOnALine.distanceTo(3, positions)
      assert(count == 39)
    }
    test("distance to point 10") - {
      val count = PointsOnALine.distanceTo(10, positions)
      assert(count == 71)
    }
  }
