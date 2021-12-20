import utest._
import utils.Grid
import cats.implicits.toShow
import utils.BinaryNumber

object ImageEnhancementTests extends TestSuite:
  val tests = Tests {

    val testGrid = Grid.buildGrid(
      """
        |#..#.
        |#....
        |##..#
        |..#..
        |..###
        |""".trim.stripMargin
    )

    val extendedGrid = testGrid.translate(1, 1)

    val enhancementAlgorithm =
      "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"

    test("points value") - {
      println(extendedGrid.show + "\n")

      val result1 = ImageEnhancement.pointValue(extendedGrid, 0 -> 0)
      assert(result1 == BinaryNumber("000000001"))

      val result2 = ImageEnhancement.pointValue(extendedGrid, 3 -> 3)
      assert(result2 == BinaryNumber("000100010"))

      val result3 = ImageEnhancement.pointValue(extendedGrid, 5 -> 5)
      assert(result3 == BinaryNumber("000110000"))

      val result4 = ImageEnhancement.pointValue(extendedGrid, 1 -> 3)
      assert(result4 == BinaryNumber("010011000"))

      val result5 = ImageEnhancement.pointValue(extendedGrid, 4 -> 2)
      assert(result5 == BinaryNumber("010000001"))
    }

    test("enhance an image once") - {
      val enhancedGrid1 =
        ImageEnhancement.enhance(extendedGrid, enhancementAlgorithm)
      println(enhancedGrid1.show + "\n")

      val enhancedGrid2 = ImageEnhancement.enhance(
        enhancedGrid1.translate(1, 1),
        enhancementAlgorithm
      )
      println(enhancedGrid2.show)

      assert(enhancedGrid2.size == 35)
    }
  }
