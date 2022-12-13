package net.bulbyvr.common

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
}

