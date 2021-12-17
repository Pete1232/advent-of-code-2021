import utest._
import utils.BinaryNumber

import math.Numeric.Implicits.infixNumericOps

object HexDecoderTests extends TestSuite:
  val tests = Tests {
    val testPacket = Packet("D2FE28")

    test("convert hex to binary") - {
      val result = testPacket.version + testPacket.typeId + testPacket.content
      assert(result == "110100101111111000101000")
    }
    test("decode a literal value") - {
      val result = testPacket.decodedContent
      assert(result == BinaryNumber("011111100101"))
      assert(result.toInt == 2021)
    }
  }
  