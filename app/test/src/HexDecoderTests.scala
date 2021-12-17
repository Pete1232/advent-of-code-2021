import utest._
import utils.BinaryNumber

import math.Numeric.Implicits.infixNumericOps

object HexDecoderTests extends TestSuite:
  val tests = Tests {
    val testPackets = Packet.fromHexString("D2FE28")

    test("decode a literal value") - {
      val result = testPackets.head.asInstanceOf[LiteralPacket].decimalValue
      assert(result == BinaryNumber("011111100101"))
      assert(result.toInt == 2021)
    }
  }
