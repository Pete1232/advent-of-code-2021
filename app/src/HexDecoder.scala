import utils.BinaryNumber
import math.Numeric.Implicits.infixNumericOps
import scala.util.Try
import cats.effect.IO

trait Packet:
  def version: Int
  def typeId: Int
  def binaryString: String
  def binarySize: Int
  def totalVersion: Int
  def decimalValue: Long

case class OperatorPacket(
    version: Int,
    typeId: Int,
    lengthTypeId: Int,
    length: Int,
    subPackets: List[Packet]
) extends Packet:

  final lazy val binaryString: String =
    Numeric[BinaryNumber].fromInt(version).paddedString(3) +
      Numeric[BinaryNumber].fromInt(typeId).paddedString(3) +
      lengthTypeId.toString + {
        if (lengthTypeId == 0)
          Numeric[BinaryNumber].fromInt(length).paddedString(15)
        else
          Numeric[BinaryNumber].fromInt(length).paddedString(11)
      } +
      subPackets.map(_.binaryString).mkString

  final lazy val binarySize: Int =
    if (lengthTypeId == 0)
      3 + 3 + 1 + 15 + length
    else
      3 + 3 + 1 + 11 + subPackets.map(_.binarySize).sum

  final lazy val totalVersion = version + subPackets.map(_.totalVersion).sum

  final lazy val decimalValue: Long = typeId match
    case 0 => subPackets.map(_.decimalValue).sum
    case 1 => subPackets.map(_.decimalValue).product
    case 2 => subPackets.map(_.decimalValue).min
    case 3 => subPackets.map(_.decimalValue).max
    case 5 => // safe to assume 2 sub-packets according to requirements
      if (subPackets.head.decimalValue > subPackets.apply(1).decimalValue) 1
      else 0
    case 6 => // safe to assume 2 sub-packets according to requirements
      if (subPackets.head.decimalValue < subPackets.apply(1).decimalValue)
        1
      else
        0
    case 7 => // safe to assume 2 sub-packets according to requirements
      if (subPackets.head.decimalValue == subPackets.apply(1).decimalValue) 1
      else 0

case class LiteralPacket(version: Int, typeId: Int, content: String)
    extends Packet:

  final lazy val binaryString: String =
    Numeric[BinaryNumber].fromInt(version).paddedString(3) +
      Numeric[BinaryNumber].fromInt(typeId).paddedString(3) +
      content

  final lazy val binarySize: Int =
    3 + 3 + content.length

  final lazy val totalVersion = version

  lazy val decimalValue: Long = BinaryNumber(
    content
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
      ._1
  ).toInt

object Packet:

  def fromHexString(hex: String): Packet =
    fromBinaryString(hexStringToBinary(hex))

  def fromBinaryString(binary: String): Packet =
    import cats.effect.unsafe.implicits.global
    getPacket(binary, top = true).unsafeRunSync()._1

  // (version:typeId:content)(version:typeId:content)(version:typeId:content)...
  def getListOfPackets(
      binary: String,
      top: Boolean,
      packetsSoFar: List[Packet] = Nil,
      maxLength: Option[Int] = None
  ): IO[List[Packet]] =
    val packetAndRemainder = getPacket(binary, false)

    packetAndRemainder
      .flatMap { case (packet, remainder) =>
        if (
          remainder.isEmpty || remainder == Some("") || remainder
            .map(_.length)
            .getOrElse(0) < 6 || Some(packetsSoFar.length) == maxLength.map(
            _ - 1
          )
        )
          IO(packetsSoFar :+ packet)
        else
          getListOfPackets(
            remainder.get,
            false,
            packetsSoFar :+ packet,
            maxLength
          )
      }
      .handleError(_ => packetsSoFar)

  @scala.annotation.tailrec
  final def readLiteralUntilDone(
      remainder: String,
      result: String = ""
  ): (String, Option[String]) =
    val next = remainder.take(5)
    if (remainder.isEmpty)
      result -> None
    else if (remainder.head == '0')
      (result + next) -> {
        if (remainder.isEmpty) None else Some(remainder.drop(5))
      }
    else
      readLiteralUntilDone(
        remainder = remainder.drop(5),
        result = result + next
      )

  // version:typeId:content
  def getPacket(binary: String, top: Boolean): IO[(Packet, Option[String])] =
    val version = BinaryNumber(binary.take(3)).toInt
    val typeId = BinaryNumber(binary.drop(3).take(3)).toInt
    if (typeId == 4)
      val content = binary.drop(6)
      val result = readLiteralUntilDone(remainder = content)
      IO(LiteralPacket(version, typeId, result._1) -> result._2)
    else
      val lengthTypeId = binary.drop(6).head.getNumericValue
      if (lengthTypeId == 0)
        val length = BinaryNumber(binary.drop(7).take(15)).toInt
        getListOfPackets(binary.drop(22).take(length), top).map { packets =>
          OperatorPacket(
            version,
            typeId,
            lengthTypeId,
            length,
            packets
          ) -> (binary.drop(22 + length) match
            case ""        => None
            case remainder => Some(remainder)
          )
        }
      else
        val length = BinaryNumber(binary.drop(7).take(11)).toInt
        getListOfPackets(binary.drop(18), top, maxLength = Some(length)).map {
          packets =>
            val resultPacket = OperatorPacket(
              version,
              typeId,
              lengthTypeId,
              length,
              packets.take(length)
            )
            resultPacket -> (binary.drop(resultPacket.binarySize) match
              case ""        => None
              case remainder => Some(remainder)
            )
        }

  def hexStringToBinary(hex: String) = hex.map(hexToBinary).mkString

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
