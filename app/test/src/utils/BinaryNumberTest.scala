package utils

import utest._
import utils.BinaryNumber

object DiagnosticReportTests extends TestSuite:
  val tests = Tests {
    test("binary to decimal") - {
      assert(BinaryNumber(List(1, 0, 1, 1, 0)).asDecimal == 22)
      assert(BinaryNumber(List(0, 1, 0, 0, 1)).asDecimal == 9)
      assert(
        BinaryNumber(
          List(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0)
        ).asDecimal == 2322
      )
    }
  }
