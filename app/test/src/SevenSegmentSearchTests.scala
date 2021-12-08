import Solutions._
import utest._

object SevenSegmentSearchTests extends TestSuite:
  val tests = Tests {
    test("decode a set of digits correctly") - {
      val display = SevenSegmentDisplay(
        List(
          "be",
          "cfbegad",
          "cbdgef",
          "fgaecd",
          "cgeb",
          "fdcge",
          "agebfd",
          "fecdb",
          "fabcd",
          "edb"
        )
      )
      print(display.toString("fdgacbe".toList: _*))
      print(display.toString("cefdb".toList: _*))
      print(display.toString("cefbgd".toList: _*))
      print(display.toString("gcbe".toList: _*))

      assert(display.toDigit("fdgacbe".toList: _*) == 8)
      assert(display.toDigit("cefdb".toList: _*) == 3)
      assert(display.toDigit("cefbgd".toList: _*) == 9)
      assert(display.toDigit("gcbe".toList: _*) == 4)
    }
  }
