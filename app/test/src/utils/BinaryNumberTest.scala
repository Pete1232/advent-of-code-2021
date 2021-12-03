package utils

import utest._
import utils.BinaryNumber

import math.Numeric.Implicits.infixNumericOps

object DiagnosticReportTests extends TestSuite:
  val tests = Tests {
    test("integer to binary") - {
      assert(
        Numeric[BinaryNumber].fromInt(22) == BinaryNumber(List(1, 0, 1, 1, 0))
      )
      println(Numeric[BinaryNumber].fromInt(9))
      assert(
        Numeric[BinaryNumber].fromInt(9) == BinaryNumber(List(1, 0, 0, 1))
      )
      assert(
        Numeric[BinaryNumber].fromInt(2322) ==
          BinaryNumber(
            List(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0)
          )
      )
    }
    test("binary to integer") - {
      assert(BinaryNumber(List(1, 0, 1, 1, 0)).toInt == 22)
      assert(BinaryNumber(List(0, 1, 0, 0, 1)).toInt == 9)
      assert(
        BinaryNumber(
          List(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0)
        ).toInt == 2322
      )
    }
  }
