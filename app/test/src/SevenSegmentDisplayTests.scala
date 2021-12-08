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
    test("figure out the digits from the string input") - {
      val input = List(
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
      )
      val displays = SevenSegmentDisplay.fromStrings(input)
      val result = displays.flatMap((display, outputs) =>
        outputs.map(output => display.toDigit(output.toList: _*))
      )

      assert(result == List(8, 3, 9, 4))
    }
    test("count 1s, 4s, 7s and 8s") - {
      val input = List(
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
        "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
        "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
        "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
        "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
        "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
        "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
        "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
        "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
        "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
      )

      val displays = SevenSegmentDisplay.fromStrings(input)

      val result = displays.flatMap((display, outputs) =>
        outputs.map(output => display.toDigit(output.toList: _*))
      )

      assert(result.count(List(1,4,7,8).contains) == 26)
    }
  }
