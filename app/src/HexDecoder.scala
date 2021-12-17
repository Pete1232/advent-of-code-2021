import utils.BinaryNumber
import math.Numeric.Implicits.infixNumericOps

case class Transmission(packet: Packet)

case class Packet(version: String, typeId: String, content: String):
  val decimalVersion: Int = BinaryNumber(version).toInt
  val decimalTypeId: Int = BinaryNumber(typeId).toInt

  val decodedContent: BinaryNumber =
    BinaryNumber(
      content
        .grouped(5)
        .foldLeft(("" -> false))((result, bits) =>
          if (result._2)
            result
          else
            val newResult = result._1 + bits.tail
            if (bits.head == '0')
              newResult -> true
            else
              newResult -> false
        )._1
    )

object Packet:
  def apply(hexString: String): Packet =
    val binaryString = hexString.map(hexToBinary).mkString
    Packet(
      binaryString.take(3),
      binaryString.drop(3).take(3),
      binaryString.drop(6)
    )

  val hexToBinary = Map(
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
