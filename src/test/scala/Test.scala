import day1.Day1
import munit.FunSuite

import scala.io.Source
import cats.implicits.*
import Day20Helper.*
import scala.collection.mutable.ArrayBuffer
import scala.util.chaining.*
import net.bulbyvr.common.* 
class Test extends FunSuite {
  test("day1") {
    val file = Source.fromResource("day1.txt").mkString
    assertEquals(Day1.run(file), 24000)
  }
  test ("day2") {
    val file = Source.fromResource("day2.txt").mkString
    assertEquals(Day2.run(file), 15)
  }
  test ("day4") {
    val file = Source.fromResource("day4.txt").mkString
    assertEquals(Day4.run(file), 2)
  }
  test ("day4Parsing") {
    val input = "1-2,3-4"
    val data =  Day4.parse(input)
    assertEquals(data.head.left, 1 to 2)
  }
  test ("day4Useless") {
    val data = Jobs(1 to 2, 3 to 4)
    assert(!data.useless)
    val data2 = Jobs(6 to 6, 4 to 6)
    assert(data2.useless)
  }
  test ("symdiff") {
    val expected = 2
    val left = 6 to 6
    val right = 4 to 6
    assertEquals(symdiff(left.toSet, right.toSet).size, expected)
    assertEquals(symdiff(right.toSet, left.toSet).size, expected)
  }
  def day20Mix(vector: Vector[Int]): Vector[Int] = {
    vector.to(ArrayBuffer).tap(mixNumbers _).toVector
  }
  /*
  test ("Day 20 Mix") {
    assertEquals(day20Mix(Vector(0, 0, 0, 1)), Vector(0, 1, 0, 0))
    assertEquals(day20Mix(Vector(0, 0, 0, 2)), Vector(0, 0, 2, 0))
    assertEquals(day20Mix(Vector(0, 0, 0, 3)), Vector(3, 0, 0, 0))
    assertEquals(day20Mix(Vector(0, 0, 0, 4)), Vector(0, 4, 0, 0))
    assertEquals(day20Mix(Vector(0, 0, 0, 5)), Vector(0, 0, 5, 0))
    assertEquals(day20Mix(Vector(0, 0, 0, 6)), Vector(6, 0, 0, 0))
    assertEquals(day20Mix(Vector(0, 0, 0, 7)), Vector(0, 7, 0, 0))
    assertEquals(day20Mix(Vector(0, 0, 0, 8)), Vector(0, 0, 8, 0))
    assertEquals(day20Mix(Vector(0, 0, 0, 9)), Vector(9, 0, 0, 0))
    assertEquals(day20Mix(Vector(0, 0, 1, 0)), Vector(1, 0, 0, 0))
    assertEquals(day20Mix(Vector(0, 2, 0, 0)), Vector(2, 0, 0, 0))
    assertEquals(day20Mix(Vector(3, 0, 0, 0)), Vector(3, 0, 0, 0))
    assertEquals(day20Mix(Vector(0, -1, 0, 0)), Vector(-1, 0, 0, 0))
    assertEquals(day20Mix(Vector(0, -2, 0, 0)), Vector(0, 0, -2, 0))
    assertEquals(day20Mix(Vector(0, -3, 0, 0)), Vector(0, -3, 0, 0))
    assertEquals(day20Mix(Vector(0, -4, 0, 0)), Vector(-4, 0, 0, 0))
    assertEquals(day20Mix(Vector(0, -5, 0, 0)), Vector(0, 0, -5, 0))
    assertEquals(day20Mix(Vector(0, -6, 0, 0)), Vector(0, -6, 0, 0))
    assertEquals(day20Mix(Vector(0, 0, -2, 0)), Vector(-2, 0, 0, 0))
    assertEquals(day20Mix(Vector(0, 0, 0, -3)), Vector(-3, 0, 0, 0))
    assertEquals(day20Mix(Vector(1, 0, 0, 0)), Vector(0, 1, 0, 0))
    assertEquals(day20Mix(Vector(2, 0, 0, 0)), Vector(0, 0, 2, 0))
    assertEquals(day20Mix(Vector(4, 0, 0, 0)), Vector(0, 4, 0, 0))
    assertEquals(day20Mix(Vector(5, 0, 0, 0)), Vector(0, 0, 5, 0))
    assertEquals(day20Mix(Vector(-1, 0, 0, 0)), Vector(0, 0, -1, 0))
    assertEquals(day20Mix(Vector(-2, 0, 0, 0)), Vector(0, -2, 0, 0))
    assertEquals(day20Mix(Vector(-3, 0, 0, 0)), Vector(-3, 0, 0, 0))

  }
  */
 /*
  test ("day 20 run") {
    val testInput = Source.fromResource("day20tst.txt").mkString 
    assertEquals(Day20Helper.run(testInput), 3L)
    //println(Day20Helper.run(Source.fromResource("day20.txt").mkString))
  } 
  test ("day 20 p2 run") {
    val testInput = Source.fromResource("day20tst.txt").mkString 
    assertEquals(Day20Helper.run2(testInput), 1623178306L)
    //println(Day20Helper.run2(Source.fromResource("day20.txt").mkString))
  }
  test ("Day 21 run") {
    assertEquals(Day21.part1(Day21.testInput), 152L)
    println(s"day 21: ${Day21.part1(Source.fromResource("day21.txt").mkString)}")
  }
  */
  /*
  test ("String Algebraic") {
    import Day21.* 
    assertEquals(x"x2", UnknownNumber(1, 2))
    assertEquals(x"2x", UnknownNumber(2, 1))
    assertEquals(x"2x2", UnknownNumber(2, 2))
    assertEquals(x"2", RealNumber(2))
    assertEquals(x"2x2 + 2x + 2", Polynomial(List(UnknownNumber(2, 2), UnknownNumber(2, 1), UnknownNumber(2, 0))))
  }
  test ("Algebraic Division") {
    import Day21.* 
    assertEquals(x"2x" / x"2", x"x")
    assertEquals(x"2x2 + 2x" / x"2", x"x2 + x")
    assertEquals(x"2x2 + 2x" / x"x", x"2x + 2")
    assertEquals(x"2x2 + 2x" / x"2x", x"x + 1")
    assertEquals(x"2" / x"4", x"1")
  }

  test ("Algebraic Multiplication") {
    import Day21.* 
    assertEquals(x"2x" * x"2", x"4x")
    assertEquals(x"2x + 2" * x"x", x"2x2 + 2x")
    assertEquals(x"2x" * x"-2", x"-4x")
    assertEquals(x"x + -3" * x"2", x"2x + -6")
  }
  test ("Algebraic Subtraction") {
    import Day21.* 
    assertEquals(x"x" - x"3", x"x + -3")
  }
  test ("Algebraic Addition") {
    import Day21.* 
    assertEquals(x"2x + -6" + x"4", x"2x + -2")
  }
  test ("Algebraic Solving") {
    import Day21.* 
    assertEquals(x"x".solveForX(1), Option(1L))
    assertEquals(x"2x".solveForX(2), Option(1L))
    assertEquals(x"2x + 2".solveForX(4), Option(1L))
  }
  */
  test ("Day 21 run 2") {
    assertEquals(Day21.part2(Day21.testInput), 301L)
    println(s"day 21 p2: ${Day21.part2(Source.fromResource("day21.txt").mkString)}")
  }
  /*
  test ("Human calc") {
    val data = Day21.parse(Day21.testInput, true)
    val op = data.monkes("ptdq")
    val sub = Day21.evaluate(data, op)
    println(s"humn - dvpt: $sub")
  }
  */

  test ("Day 22 Grid Traversal") {
    import Day22.*
    import Vec2i.*
    val grid = ForbiddenGrid(Grid.fill(10, 10)(GridPos.Open)
      .updated(v2i"9, 9")(GridPos.Solid)
      .updated(v2i"1, 1")(GridPos.Solid))
    // Right overflow
    assertEquals(grid.move(v2i"0, 0", Direction2D.Right, 10), v2i"0, 0")
    // Down overflow
    assertEquals(grid.move(v2i"0, 0", Direction2D.Down, 10), v2i"0, 0")
    // Down Stopping 
    assertEquals(grid.move(v2i"9, 0", Direction2D.Down, 9), v2i"9, 8")
    // Right stopping 
    assertEquals(grid.move(v2i"0, 9", Direction2D.Right, 10), v2i"8, 9")
    // Left Stopping 
    assertEquals(grid.move(v2i"9, 1", Direction2D.Left, 10), v2i"2, 1")
    // Left overflow 
    assertEquals(grid.move(v2i"9, 0", Direction2D.Left, 10), v2i"9, 0")
    // Up overflow
    assertEquals(grid.move(v2i"0, 9", Direction2D.Up, 10), v2i"0, 9")
    // Up stopping
    assertEquals(grid.move(v2i"1, 9", Direction2D.Up, 10), v2i"1, 2")

    
  }
  /*
  test ("Day 22 Part 1") {
    import Day22.* 

    val testInput = Source.fromResource("day22tst.txt").mkString 
    assertEquals(Day22.run(testInput), 6032)

    println(s"day 22 p1: ${Day22.run(Source.fromResource("day22.txt").mkString)}")
  }
  test ("Day 23 part 1") {
    assertEquals(Day23.run(Day23.testInput), 110)
  }
  */
  /*
  test ("Day 24 part 1") {
    val testInput = Source.fromResource("day24tst.txt").mkString 
    assertEquals(Day24.run(testInput), 18)

    // println(s"day 24 p1: ${Day24.run(Source.fromResource("day24.txt").mkString)}")
  }
  */

  test ("SNAFU Parsing") {
    import Day25.snafuToDec
    assertEquals(snafuToDec("1"), 1L)
    assertEquals(snafuToDec("2"), 2L)
    assertEquals(snafuToDec("1="), 3L)
    assertEquals(snafuToDec("1-"), 4L)
    assertEquals(snafuToDec("10"), 5L)
    assertEquals(snafuToDec("11"), 6L)
    assertEquals(snafuToDec("12"), 7L)
    assertEquals(snafuToDec("2="), 8L)
    assertEquals(snafuToDec("2-"), 9L)
    assertEquals(snafuToDec("20"), 10L)
    assertEquals(snafuToDec("1=0"), 15L)
    assertEquals(snafuToDec("1-0"), 20L)
    assertEquals(snafuToDec("1=11-2"), 2022L)
    
  }
  test ("SNAFU Encoding") {
    import Day25.decToSnafu 
    assertEquals(decToSnafu(1), "1")
    assertEquals(decToSnafu(2), "2")
    assertEquals(decToSnafu(3), "1=")
    assertEquals(decToSnafu(4), "1-")
    assertEquals(decToSnafu(5), "10")
    assertEquals(decToSnafu(6), "11")
    assertEquals(decToSnafu(7), "12")
    assertEquals(decToSnafu(8), "2=")
    assertEquals(decToSnafu(9), "2-")
    assertEquals(decToSnafu(10), "20")
    assertEquals(decToSnafu(15), "1=0")
    assertEquals(decToSnafu(20), "1-0")
    assertEquals(decToSnafu(2022), "1=11-2")
  }
  test ("Day 25 P1") {
    val tstInput = Source.fromResource("day25tst.txt").mkString 
    val tstData = Day25.parse(tstInput)
    assertEquals(tstData.sum, 4890L)
    assertEquals(Day25.run(Source.fromResource("day25tst.txt").mkString), "2=-1=0")
    println(s"Day 25 p1: ${Day25.run(Source.fromResource("day25.txt").mkString)}")
  }

}
