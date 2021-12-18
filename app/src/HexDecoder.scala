import utils.BinaryNumber
import math.Numeric.Implicits.infixNumericOps

trait Packet:
  def version: Int
  def typeId: Int

case class OperatorPacket(
    version: Int,
    typeId: Int,
    lengthTypeId: Int,
    length: Int,
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

  def fromHexString(hex: String): Packet =
    fromBinaryString(hex.map(hexToBinary).mkString)

  def fromBinaryString(binary: String): Packet =
    val version = BinaryNumber(binary.take(3)).toInt
    val typeId = BinaryNumber(binary.drop(3).take(3)).toInt
    apply(version, typeId, binary.drop(6))

  def buildSubPackets(binary: String): List[Packet] =
    // TODO the duplication here doesn't feel right
    val packetVersion = BinaryNumber(binary.take(3)).toInt
    val packetTypeId = BinaryNumber(binary.drop(3).take(3)).toInt
    buildPackets(packetVersion, packetTypeId, binary.drop(6))

  private def buildPackets(
      packetVersion: Int,
      packetTypeId: Int,
      remainingContent: String,
      packetString: String = ""
  ): List[Packet] =
    val chunk = remainingContent.take(5)
    if (chunk.length < 5)
      // nothing left to parse
      List.empty
    else if (chunk.head == '0')
      // end of the packet, but maybe not the whole message
      Packet(
        packetVersion,
        packetTypeId,
        packetString + chunk
      ) +: buildSubPackets(remainingContent.drop(5))
    else
      // the chunk is valid so continue to build up the packet
      buildPackets(
        packetVersion,
        packetTypeId,
        remainingContent.drop(5),
        packetString + chunk
      )

  def apply(version: Int, typeId: Int, content: String): Packet =
    if (typeId == 4) LiteralPacket(version, typeId, content)
    else
      val lengthTypeId = content.head.getNumericValue
      if (lengthTypeId == 0)
        val length = BinaryNumber(content.tail.take(15)).toInt
        OperatorPacket(
          version,
          typeId,
          lengthTypeId,
          length,
          buildSubPackets(content.drop(16).take(length))
        )
      else
        val length = BinaryNumber(content.tail.take(11)).toInt
        OperatorPacket(
          version,
          typeId,
          lengthTypeId,
          length,
          buildSubPackets(content.drop(12)).take(length)
        )

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
