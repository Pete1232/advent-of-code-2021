package utils

import utest._
import utils.BinaryNumber

import math.Numeric.Implicits.infixNumericOps

object DiagnosticReportTests extends TestSuite:
  val tests = Tests {
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
