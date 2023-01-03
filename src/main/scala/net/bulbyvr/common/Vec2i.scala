package net.bulbyvr.common
import math.Ordering.Implicits.infixOrderingOps
case class Vec2i(x: Int, y: Int) extends Ordered[Vec2i] {
  def offset(dir: Direction2D, n: Int = 1) = 
    dir match
      case Direction2D.Up => this.copy(y = y + n)
      case Direction2D.Down => this.copy(y = y - n)
      case Direction2D.Left => this.copy(x = x - n)
      case Direction2D.Right => this.copy(x = x + n)
  override def compare(that: Vec2i): Int = {
    if (this.x - that.x != 0) 
      this.x - that.x 
    else 
      this.y - that.y
  }
  final def taxiDistance(that: Vec2i): Int = {
    Math.abs(this.x - that.x) + Math.abs(this.y - that.y)
  }
  def straightLine(that: Vec2i): List[Vec2i] = {
    require(this.x == that.x || this.y == that.y)
    val shouldReverse = (this `max` that) == this 
    def maybeReverse[A](ls: List[A]): List[A] = {
      if (shouldReverse)
        ls.reverse 
      else 
        ls
    }
    if (this.x == that.x) {
      val minY = this.y `min` that.y 
      val maxY = this.y `max` that.y 
      maybeReverse((minY to maxY).map(yy => Vec2i(this.x, yy)).toList)
    } else { 
      val minX = this.x `min` that.x 
      val maxX = this.x `max` that.x 
      maybeReverse((minX to maxX).map(xx => Vec2i(xx, this.y)).toList)
    }
  }
}

object Vec2i {
  extension (sc: StringContext) {
    def v2i(args: Any*) = {
      require(args.isEmpty)
      val List(x, y) = sc.parts(0).split(",").toList
      Vec2i(x.trim.toInt, y.trim.toInt)
    }
  }
}
