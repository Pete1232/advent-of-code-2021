import utils.BinaryNumber
import math.Numeric.Implicits.infixNumericOps

trait Packet:
  def version: Int
  def typeId: Int

case class OperatorPacket(
    version: Int,
    typeId: Int,
    lengthTypeId: Int,
    subPackets: List[Packet]
) extends Packet

case class LiteralPacket(version: Int, typeId: Int, content: String)
    extends Packet:
  lazy val decimalValue = BinaryNumber(
    content
      .grouped(5)
      .foldLeft(("" -> false))((result, bits) =>
        if (result._2) result
        else
          val newResult = result._1 + bits.tail
          if (bits.head == '0')
            newResult -> true
          else
            newResult -> false
      )
      ._1
  )

object Packet:

  def fromHexString(hex: String): List[Packet] =
    fromBinaryString(hex.map(hexToBinary).mkString)

  def fromBinaryString(binary: String): List[Packet] =
    val version = BinaryNumber(binary.take(3)).toInt
    val typeId = BinaryNumber(binary.drop(3).take(3)).toInt
    buildPackets(version, typeId, binary.drop(6))

  def apply(version: Int, typeId: Int, content: String): Packet =
    if (typeId == 4) LiteralPacket(version, typeId, content)
    else
      val lengthTypeId = content.head.toInt
      if (lengthTypeId == 0)
        // todo implement length limit
        val length = BinaryNumber(content.tail.take(15)).toInt
        OperatorPacket(
          version,
          typeId,
          lengthTypeId,
          fromBinaryString(content.drop(16))
        )
      else
        // todo implement sub-packet count
        OperatorPacket(
          version,
          typeId,
          lengthTypeId,
          fromBinaryString(content.drop(16))
        )

  private def buildPackets(
      version: Int,
      typeId: Int,
      content: String,
      packetString: String = ""
  ): List[Packet] =
    val chunk = content.take(5)
    if (chunk.length < 5)
      List(Packet(version, typeId, packetString + chunk))
    else if (chunk.head == 0)
      Packet(version, typeId, packetString + chunk) +: fromBinaryString(
        content.drop(5)
      )
    else
      buildPackets(version, typeId, content.drop(5), packetString + chunk)

  private val hexToBinary = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )
