import utils.BinaryNumber
import math.Numeric.Implicits.infixNumericOps
import scala.util.Try

trait Packet:
  def version: Int
  def typeId: Int
  def binarySize: Int
  def totalVersion: Int

case class OperatorPacket(
    version: Int,
    typeId: Int,
    lengthTypeId: Int,
    length: Int,
    subPackets: List[Packet]
) extends Packet:
  final val binarySize: Int =
    if (lengthTypeId == 0)
      3 + 3 + 1 + 15 + length
    else
      3 + 3 + 1 + 11 + subPackets.map(_.binarySize).sum

  final val totalVersion = version + subPackets.map(_.version).sum

case class LiteralPacket(version: Int, typeId: Int, content: String)
    extends Packet:

  final val binarySize = 3 + 3 + content.length

  final val totalVersion = version

  lazy val decimalValue = BinaryNumber(
    content
  )

object Packet:

  def fromHexString(hex: String): Packet =
    fromBinaryString(hex.map(hexToBinary).mkString)

  def fromBinaryString(binary: String): Packet =
    // val version = BinaryNumber(binary.take(3)).toInt
    // val typeId = BinaryNumber(binary.drop(3).take(3)).toInt
    // apply(version, typeId, binary.drop(6))._1
    getPacket(binary)._1

  // (version:typeId:content)(version:typeId:content)(version:typeId:content)...
  def getListOfPackets(binary: String): List[Packet] =
    val packetAndRemainder = Try(getPacket(binary)).toOption

    packetAndRemainder match
      case None => List.empty
      case Some((packet, remainder)) =>
        if (remainder.isEmpty || remainder == Some(""))
          List(packet)
        else
          packet +: getListOfPackets(remainder.get)

  // version:typeId:content
  def getPacket(binary: String): (Packet, Option[String]) =
    val version = BinaryNumber(binary.take(3)).toInt
    val typeId = BinaryNumber(binary.drop(3).take(3)).toInt
    if (typeId == 4)
      val content = binary.drop(6)
      val result = content
        .grouped(5)
        .foldLeft[(String, Option[String])](("" -> None))((result, bits) =>
          if (result._2.isDefined)
            result._1 -> (result._2 match
              case None            => Some(bits)
              case Some(remainder) => Some(remainder + bits)
            )
          else
            val newResult = result._1 + bits.tail
            if (bits.head == '0')
              newResult -> Some("")
            else
              newResult -> None
        )
      LiteralPacket(version, typeId, result._1) -> result._2
    else
      val lengthTypeId = binary.drop(6).head.getNumericValue
      if (lengthTypeId == 0)
        val length = BinaryNumber(binary.drop(7).take(15)).toInt
        OperatorPacket(
          version,
          typeId,
          lengthTypeId,
          length,
          getListOfPackets(binary.drop(22).take(length))
        ) -> (binary.drop(22 + length) match
          case ""        => None
          case remainder => Some(remainder)
        )
      else
        val length = BinaryNumber(binary.drop(7).take(11)).toInt
        val resultPacket = OperatorPacket(
          version,
          typeId,
          lengthTypeId,
          length,
          getListOfPackets(binary.drop(18)).take(length)
        )
        resultPacket -> (binary.drop(resultPacket.binarySize) match
          case ""        => None
          case remainder => Some(remainder)
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
