import utest._
import utils.BinaryNumber

import math.Numeric.Implicits.infixNumericOps

object HexDecoderTests extends TestSuite:
  val tests = Tests {
    lazy val testPacket = Packet.fromHexString("D2FE28")

    test("decode a literal value") - {
      val result = testPacket.asInstanceOf[LiteralPacket].decimalValue
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

      assert(testPacket.binarySize == testPacket.binaryString.length)
      assert(Packet.fromBinaryString(testPacket.binaryString) == testPacket)
    }

    test("operator value 0 and back again") - {

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

    test("operator with sub literal and operator") - {

      // first part of puzzle up to where it fails
      val result = Packet.fromBinaryString(
        "100 000 1 00000000010".replaceAll("\\s", "") + // v=4,t=0,lt=1,l=2
          "00101010000000000111101110000000000111001000000001100100001100101000000000010100100000000101101000001010100000000001001001100000000001010011000000010010111011101100000001000110000110110000000100000010100010000000001110110001100110000000000110100100000000110001000100100000000010101110100010100000000001000001000000001000011011001100000000011100001010010000000001011010011000000000000100010011001000000000001011100001001011010111110001001110111111111100000000" +
          "01110000100"
      )

      assert(result.asInstanceOf[OperatorPacket].subPackets.size == 2)
      assert(
        result
          .asInstanceOf[OperatorPacket]
          .subPackets
          .apply(1)
          .asInstanceOf[LiteralPacket] == LiteralPacket(3, 4, "00100")
      )
    }

    test("value tests") - {
      val result1 = Packet.fromHexString("C200B40A82").decimalValue
      assert(result1 == 3)
      val result2 = Packet.fromHexString("04005AC33890").decimalValue
      assert(result2 == 54)
      val result3 = Packet.fromHexString("880086C3E88112").decimalValue
      assert(result3 == 7)
      val result4 = Packet.fromHexString("CE00C43D881120").decimalValue
      assert(result4 == 9)
      val result5 = Packet.fromHexString("D8005AC2A8F0").decimalValue
      assert(result5 == 1)
      val result6 = Packet.fromHexString("F600BC2D8F").decimalValue
      assert(result6 == 0)
      val result7 = Packet.fromHexString("9C005AC2F8F0").decimalValue
      assert(result7 == 0)
      val result8 =
        Packet.fromHexString("9C0141080250320F1802104A08").decimalValue
      assert(result8 == 1)
    }

    test("answer") - {
      val result = Packet.fromHexString(
        "820D4A801EE00720190CA005201682A00498014C04BBB01186C040A200EC66006900C44802BA280104021B30070A4016980044C800B84B5F13BFF007081800FE97FDF830401BF4A6E239A009CCE22E53DC9429C170013A8C01E87D102399803F1120B4632004261045183F303E4017DE002F3292CB04DE86E6E7E54100366A5490698023400ABCC59E262CFD31DDD1E8C0228D938872A472E471FC80082950220096E55EF0012882529182D180293139E3AC9A00A080391563B4121007223C4A8B3279B2AA80450DE4B72A9248864EAB1802940095CDE0FA4DAA5E76C4E30EBE18021401B88002170BA0A43000043E27462829318F83B00593225F10267FAEDD2E56B0323005E55EE6830C013B00464592458E52D1DF3F97720110258DAC0161007A084228B0200DC568FB14D40129F33968891005FBC00E7CAEDD25B12E692A7409003B392EA3497716ED2CFF39FC42B8E593CC015B00525754B7DFA67699296DD018802839E35956397449D66997F2013C3803760004262C4288B40008747E8E114672564E5002256F6CC3D7726006125A6593A671A48043DC00A4A6A5B9EAC1F352DCF560A9385BEED29A8311802B37BE635F54F004A5C1A5C1C40279FDD7B7BC4126ED8A4A368994B530833D7A439AA1E9009D4200C4178FF0880010E8431F62C880370F63E44B9D1E200ADAC01091029FC7CB26BD25710052384097004677679159C02D9C9465C7B92CFACD91227F7CD678D12C2A402C24BF37E9DE15A36E8026200F4668AF170401A8BD05A242009692BFC708A4BDCFCC8A4AC3931EAEBB3D314C35900477A0094F36CF354EE0CCC01B985A932D993D87E2017CE5AB6A84C96C265FA750BA4E6A52521C300467033401595D8BCC2818029C00AA4A4FBE6F8CB31CAE7D1CDDAE2E9006FD600AC9ED666A6293FAFF699FC168001FE9DC5BE3B2A6B3EED060"
      )

      assert(result.totalVersion == 974)
      assert(result.decimalValue == 180616437720L)
    }
  }
