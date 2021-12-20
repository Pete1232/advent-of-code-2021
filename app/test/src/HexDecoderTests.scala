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
      val subPackets =
        result.subPackets.map(_.asInstanceOf[LiteralPacket].decimalValue.toInt)
      assert(result.length == 27)
      assert(subPackets == List(10, 20))
    }

    lazy val testOperation2 = Packet.fromHexString("EE00D40C823060")

    test("decode an operator of length type 1") - {
      val result = testOperation2.asInstanceOf[OperatorPacket]
      val subPackets =
        result.subPackets.map(_.asInstanceOf[LiteralPacket].decimalValue.toInt)
      assert(result.length == 3)
      assert(subPackets == List(1, 2, 3))
    }

    test("nested packets 1") - {
      val result =
        Packet.fromHexString("8A004A801A8002F478").asInstanceOf[OperatorPacket]
      val subPacket1 = result.subPackets.head.asInstanceOf[OperatorPacket]
      val subPacket2 = subPacket1.subPackets.head.asInstanceOf[OperatorPacket]
      val subPacket3 = subPacket2.subPackets.head.asInstanceOf[LiteralPacket]

      assert(result.version == 4)
      assert(subPacket1.version == 1)
      assert(subPacket2.version == 5)
      assert(subPacket3.version == 6)
    }

    test("nested packets 2") - {
      val result = Packet
        .fromHexString("620080001611562C8802118E34")
        .asInstanceOf[OperatorPacket]
      val subPacket1 = result.subPackets.head.asInstanceOf[OperatorPacket]
      val subPacket2 = result.subPackets.last.asInstanceOf[OperatorPacket]

      val literalPacket1_1 =
        subPacket1.subPackets.head.asInstanceOf[LiteralPacket]
      val literalPacket1_2 =
        subPacket1.subPackets.last.asInstanceOf[LiteralPacket]

      val literalPacket2_1 =
        subPacket2.subPackets.head.asInstanceOf[LiteralPacket]
      val literalPacket2_2 =
        subPacket2.subPackets.last.asInstanceOf[LiteralPacket]

      assert(result.version == 3)
      assert(result.typeId == 0)
      assert(result.lengthTypeId == 1)
      assert(result.length == 2)

      assert(subPacket1.version == 0)
      assert(subPacket1.typeId == 0)
      assert(subPacket1.lengthTypeId == 0)
      assert(subPacket1.length == 22)

      assert(subPacket2.version == 1)
      assert(subPacket2.typeId == 0)
      assert(subPacket2.lengthTypeId == 1)
      assert(subPacket2.length == 2)

      assert(result.version == 3)
      val versionCount = result.totalVersion
      assert(versionCount == 12)
    }

    test("nested packets 3") - {
      val result = Packet
        .fromHexString("C0015000016115A2E0802F182340")
        .asInstanceOf[OperatorPacket]
      val subPacket1 = result.subPackets.head.asInstanceOf[OperatorPacket]
      val subPacket2 = result.subPackets.last.asInstanceOf[OperatorPacket]

      val literalPacket1_1 =
        subPacket1.subPackets.head.asInstanceOf[LiteralPacket]
      val literalPacket1_2 =
        subPacket1.subPackets.last.asInstanceOf[LiteralPacket]

      val literalPacket2_1 =
        subPacket2.subPackets.head.asInstanceOf[LiteralPacket]
      val literalPacket2_2 =
        subPacket2.subPackets.last.asInstanceOf[LiteralPacket]

      val versionCount = result.totalVersion
      assert(versionCount == 23)
    }

    test("nested packets 4") - {
      val result = Packet
        .fromHexString("A0016C880162017C3686B18A3D4780")
        .asInstanceOf[OperatorPacket]
      val subPacket1 = result.subPackets.head.asInstanceOf[OperatorPacket]
      val subPacket2 = subPacket1.subPackets.last.asInstanceOf[OperatorPacket]
      val literalPackets =
        subPacket2.subPackets.map(_.asInstanceOf[LiteralPacket])

      assert(literalPackets.size == 5)
      val versionCount = result.totalVersion
      assert(versionCount == 31)
    }

    test("literal value and back again") - {
      val testPacket = LiteralPacket(
        version = 5,
        typeId = 4,
        content = "1" + Numeric[BinaryNumber]
          .fromInt(5)
          .paddedString(4) + "1" + Numeric[BinaryNumber]
          .fromInt(15)
          .paddedString(4) + Numeric[BinaryNumber]
          .fromInt(5)
          .paddedString(5)
      )

      assert(Packet.fromBinaryString(testPacket.binaryString) == testPacket)
    }

    test("operator value 1 and back again") - {

      val testSubPacket: Packet = LiteralPacket(
        version = 5,
        typeId = 4,
        content = "1" + Numeric[BinaryNumber]
          .fromInt(5)
          .paddedString(4) + "1" + Numeric[BinaryNumber]
          .fromInt(15)
          .paddedString(4) + Numeric[BinaryNumber]
          .fromInt(5)
          .paddedString(5)
      )

      val testPacket = OperatorPacket(
        version = 5,
        typeId = 2,
        lengthTypeId = 0,
        length = testSubPacket.binarySize,
        subPackets = List(testSubPacket)
      )

      assert(testPacket.binarySize == testPacket.binaryString.length)
      assert(Packet.fromBinaryString(testPacket.binaryString) == testPacket)
    }
  }
