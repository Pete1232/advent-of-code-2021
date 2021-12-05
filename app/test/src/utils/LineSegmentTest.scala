package utils

import utest._
import utils.LineSegment

object LineSegmentTests extends TestSuite:
  val tests = Tests {
    test("points on a horizontal line") - {
      assert(
        LineSegment(
          (0, 9),
          (5, 9)
        ).points == List(
          (0, 9),
          (1, 9),
          (2, 9),
          (3, 9),
          (4, 9),
          (5, 9)
        )
      )
    }
    test("points on a horizontal line in reverse") - {
      assert(
        LineSegment(
          (5, 9),
          (0, 9)
        ).points == List(
          (0, 9),
          (1, 9),
          (2, 9),
          (3, 9),
          (4, 9),
          (5, 9)
        )
      )
    }
    test("points on a vertical line") - {
      assert(
        LineSegment(
          (7, 0),
          (7, 4)
        ).points == List(
          (7, 0),
          (7, 1),
          (7, 2),
          (7, 3),
          (7, 4)
        )
      )
    }
    test("points on a vertical line in reverse") - {
      assert(
        LineSegment(
          (7, 4),
          (7, 0)
        ).points == List(
          (7, 4),
          (7, 3),
          (7, 2),
          (7, 1),
          (7, 0)
        )
      )
    }
  }
