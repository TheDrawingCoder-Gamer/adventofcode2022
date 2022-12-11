import day1.Day1
import scala.io.Source

@main def main() : Unit = {
  runD4P2()
}
def runDay1Part1() : Unit = {
  val input = Source.fromResource("day1.txt").mkString
  println(Day1.run(input))
}

def runDay1Part2() : Unit = {
  val input = Source.fromResource("day1.txt").mkString
  println(Day1.runP2(input))
}

def runDay2P1() : Unit = {
  val input = Source.fromResource("day2.txt").mkString
  println(Day2.run(input))
}

def runDay2P2() : Unit = {
  val input = Source.fromResource("day2.txt").mkString
  println(Day2.runP2(input))
}
def runDay3P1() : Unit = {
  val input = Source.fromResource("day3.txt").mkString
  println(Day3.run(input))
}
def runDay3P2() : Unit = {
  val input = Source.fromResource("day3.txt").mkString
  println(Day3.runP2(input))
}
def runD4P1() : Unit = {
  val input = Source.fromResource("day4.txt").mkString
  println(Day4.run(input))
}
def runD4P2() : Unit = {
  val input = Source.fromResource("day4.txt").mkString
  println(Day4.runP2(input))
}
