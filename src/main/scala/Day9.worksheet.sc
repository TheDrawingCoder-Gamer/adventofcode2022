import cats.implicits.* 
import cats.data.State
import net.bulbyvr.common.{Grid, Direction2D, Vec2i, Axis2D, given}
import scala.jdk.StreamConverters.*
import scala.io.Source

case class HeadTailPos(head: Vec2i, tail: Vec2i)

type Day9State[A] = State[List[Vec2i], A]
def manhattanDistance(a: Vec2i, b: Vec2i) = Math.abs(a.x - b.x) + Math.abs(a.y - b.y)
def minDistance(a: Vec2i, b: Vec2i) = Math.min(Math.abs(a.x - b.x), Math.abs(a.y - b.y))
def maxDistance(a: Vec2i, b: Vec2i) = Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y))
def minAxis(a: Vec2i, b: Vec2i) = List((Math.abs(a.x - b.x), Axis2D.X), (Math.abs(a.y - b.y), Axis2D.Y)).maxBy(_._1)._2
// @annotation.tailrec
final def correctTail(poses: List[Vec2i]): List[Vec2i] = {
  poses match {
    case head :: (tail :: next) if maxDistance(head, tail) <= 1 => head :: correctTail(poses.tail) 
    case head :: (tail :: next) =>
        val newTail = 
          if (head.x == tail.x) {
            val y = if (head.y > tail.y) 1 else -1 
            tail.copy(y = tail.y + y)
          } else if (head.y == tail.y) {
            val x = if (head.x > tail.x) 1 else -1 
            tail.copy(x = tail.x + x)
          } else {
            val x = if (head.x > tail.x) 1 else -1 
            val y = if (head.y > tail.y) 1 else -1
            Vec2i(tail.x + x, tail.y + y)
          }
        head :: correctTail(newTail :: next)

    case _ => poses 
  }
}
def move(dir: Direction2D): Day9State[List[Vec2i]] = State { case head :: next => 
  val newHead = dir match {
    case Direction2D.Down => head.copy(y = head.y - 1)
    case Direction2D.Left => head.copy(x = head.x - 1)
    case Direction2D.Right => head.copy(x = head.x + 1)
    case Direction2D.Up => head.copy(y = head.y + 1)
  }
  val r = correctTail(newHead :: next)
  (r, r)
}

def moveN(dir: Direction2D, n: Int): Day9State[List[List[Vec2i]]] = move(dir).replicateA(n)

case class Movement(dir: Direction2D, n: Int) {
  val execute: Day9State[List[List[Vec2i]]] = moveN(dir, n)
}

def parse(input: String): List[Movement] = {
  input.lines().map { it => 
    val (l, r) = it.splitAt(1)
    println(r)
    val n = r.drop(1).toInt 
    val d = l(0) match {
      case 'R' => Direction2D.Right 
      case 'U' => Direction2D.Up 
      case 'D' => Direction2D.Down 
      case 'L' => Direction2D.Left 
      case _ => throw IllegalArgumentException()
    }
    Movement(d, n)
  }.toScala(List)
}

val input = Source.fromResource("day9.txt").mkString 
val movements = parse(input)
movements.traverse(_.execute).map(_.flatten).map(_.map(_.tail)).runA(List.fill(10)(Vec2i(0, 0))).value.map(_.last).toSet.size
