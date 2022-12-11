package day1

import cats.implicits.*
import net.bulbyvr.common.TraverseExt.*
import scala.util.chaining.*
import scala.jdk.StreamConverters.*
object Day1 {
  def parse(str : String) : List[List[Int]] = {
    val elfs = str.lines().toScala(List).splitOn("")
    elfs.toList.map(_.map(_.toInt).toList)
  }
  def run(input : String) : Int = {
    val elfs = parse(input)
    val summedElfs = elfs.map(it => it.sum)

    summedElfs.max
  }
  def runP2(input : String) : Int = {
    val elfs = parse(input)
    val summedElfs : List[Int] = elfs.map(it => it.sum).sorted.reverse
    summedElfs.take(3).sum
  }
}
