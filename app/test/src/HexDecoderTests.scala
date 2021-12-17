import utest._
import utils.BinaryNumber

import math.Numeric.Implicits.infixNumericOps

object HexDecoderTests extends TestSuite:
  val tests = Tests {
    lazy val testPacket = Packet.fromHexString("D2FE28")

    test("decode a literal value") - {
      val result = testPacket.asInstanceOf[LiteralPacket].decimalValue
      assert(result == BinaryNumber("011111100101"))
      assert(result.toInt == 2021)
    }

    lazy val testOperation1 = Packet.fromHexString("38006F45291200")

    test("decode an operation of length type 0") - {
      val result = testOperation1.asInstanceOf[OperatorPacket]
      val subPackets = result.subPackets.map(_.asInstanceOf[LiteralPacket].decimalValue.toInt)
      assert(result.length == 27)
      assert(subPackets == List(10, 20))
    }

    lazy val testOperation2 = Packet.fromHexString("EE00D40C823060")

    test("decode an operation of length type 1") - {
      val result = testOperation2.asInstanceOf[OperatorPacket]
      println(result.subPackets)
      val subPackets = result.subPackets.map(_.asInstanceOf[LiteralPacket].decimalValue.toInt)
      assert(result.length == 3)
      assert(subPackets == List(1, 2, 3))
    }
  }
