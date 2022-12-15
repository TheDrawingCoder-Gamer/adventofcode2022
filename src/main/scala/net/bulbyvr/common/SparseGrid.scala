package net.bulbyvr.common

case class SparseGrid[A](values: Map[Vec2i, A], default: A) {
  def apply(p: Vec2i): A = 
    values.getOrElse(p, default)
  def apply(x: Int, y: Int): A = 
    apply(Vec2i(x, y))
}
