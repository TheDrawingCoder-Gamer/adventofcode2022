import net.bulbyvr.common.{Grid, Vec2i, Direction2D}
import cats.*
import cats.implicits.*
import scala.io.Source 
import scala.collection.mutable as mut 
import scala.jdk.StreamConverters.*

extension (a: Int) { 
  infix def ascendsTo(b: Int): Range = {
    val s = a `min` b 
    val e = b `max` a 

    s to e
  }
}
case class Pathway(points: List[Vec2i]) {
  def bake: Set[Vec2i] = 
    Set.from(points.sliding(2).map { case List(s, e) => 
      require(s.x == e.x || s.y == e.y)
      for {
        y <- s.y ascendsTo e.y
        x <- s.x ascendsTo e.x
      } yield Vec2i(x, y)
    }.flatten)
}

val source = Vec2i(500, 0)


enum CavePoint {
  case Rock, Sand, Air
}
// PROBABLY UNOPTIMIZED LOL
class LazyLineLattice(pathways: Vector[Pathway]) {
  private val testedPoints: mut.HashMap[Vec2i, Boolean] = mut.HashMap()

  def apply(p: Vec2i) = 
    testedPoints.getOrElseUpdate(p, pathways.exists(_.points.contains(p)))
}

case class SparseCaveGrid(points: Map[Vec2i, CavePoint], source: Vec2i) {
  val lowestPoint: Vec2i = points.maxBy( (p, c) => if (c == CavePoint.Rock) p.y else 0)._1
  println(lowestPoint.y)
  val floor = lowestPoint.y + 2
  def apply(p: Vec2i) = 
    points.getOrElse(p, if (p.y >= floor) CavePoint.Rock else CavePoint.Air)
  @annotation.tailrec
  final def moveSand(p: Vec2i): Option[Vec2i] = {
    if (apply(p) != CavePoint.Air) return None
    // y is flipped
    val belowPos = p.copy(y = p.y + 1)
    val berightPos = belowPos.copy(x = p.x + 1)
    val beleftPos = belowPos.copy(x = p.x - 1)
    
    val below = apply(belowPos)
    lazy val belowRight = apply(berightPos)
    lazy val belowLeft = apply(beleftPos)
    if (below == CavePoint.Air) 
      moveSand(belowPos)
    else if (belowLeft == CavePoint.Air) 
      moveSand(beleftPos)
    else if (belowRight == CavePoint.Air)
      moveSand(berightPos)
    else 
      Some(p)
  }
  def withSand: Option[SparseCaveGrid] =
    moveSand(source).map { it => 
      SparseCaveGrid(points.updated(it, CavePoint.Sand), source)
    }
  def show: String = {
    val rightBound = points.maxBy( (p, _) => p.x)._1.x 
    val leftBound = points.minBy( (p, _) => p.x)._1.x 
    val topBound = 0 
    val bottomBound = floor 
    (for {
      y <- topBound to bottomBound
    } yield {
      (for {
        x <- leftBound to rightBound 
      } yield
        apply(Vec2i(x, y)) match
          case CavePoint.Rock => '#'
          case CavePoint.Sand => 'o'
          case CavePoint.Air => '.'
      ).mkString 
    }).mkString("", "\n", "")
  }
}
object SparseCaveGrid {
  def apply(rock: Set[Vec2i], source: Vec2i): SparseCaveGrid = {
    SparseCaveGrid(Map.from(rock.zip(Vector.fill(rock.size)(CavePoint.Rock))), source)
  }
}
def parse(input: String): Vector[Pathway] = {
  input.lines().map { it =>
    Pathway(it.split("->").map(_.trim).map { s => 
      s match {
        case s"$l,$r" => Vec2i(l.toInt, r.toInt)
      }
    }.toList)
  }.toScala(Vector)
}

val input = Source.fromResource("day14.txt").mkString 
val data = parse(input)
val goodData = data.map(_.bake).flatten.toSet
val daGrid = SparseCaveGrid(goodData, source)
LazyList.iterate(Option(daGrid))(_.map(_.withSand).flatten).takeWhile(_.isDefined).size - 1
