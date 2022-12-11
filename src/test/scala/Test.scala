import day1.Day1
import munit.FunSuite

import scala.io.Source
import cats.implicits.*
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
}
