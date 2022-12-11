import scala.jdk.StreamConverters.*
import net.bulbyvr.common.TraverseExt.*
import cats.implicits.*
import cats.{Applicative, Traverse}
object Day4 {
  def parse(input : String) : List[Jobs] = {
    input.lines().map { it =>
      Jobs.parse(it)
    }.toScala(List)
  }
  def run(input : String) : Int = {
    val data = parse(input)
    data.count(_.useless)
  }
  def runP2(input : String) : Int = {
    val data = parse(input)
    data.count(_.anyMatch)
  }

}
def symdiff[A](l : Set[A], r : Set[A]) = {
  (l ++ r) diff (l intersect r)
}
case class Jobs(left : Range, right : Range) {
  def useless : Boolean = {
    val size = symdiff(left.toSet, right.toSet).size
    val expected = (left.size max right.size) - (left.size min right.size)
    size == expected
  }
  def anyMatch : Boolean = {
    val size = symdiff(left.toSet, right.toSet).size 
    val expected = left.size + right.size 
    // if any match between expected will be too high
    size != expected 
  }
}

object Jobs {
  def parse(input : String) : Jobs = {
    val parts = input.split(',')
    val l = parts(0)
    val r = parts(1)
    Jobs(parseRange(l), parseRange(r))

  }
  private def parseRange(input : String) : Range = {
    val parts = input.trim().split('-').take(2)
    val l = parts(0)
    val r = parts(1)
    l.toInt to r.toInt
  }
}
