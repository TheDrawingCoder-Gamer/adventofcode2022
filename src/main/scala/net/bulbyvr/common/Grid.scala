package net.bulbyvr.common

import cats.implicits.*
import cats.Show
import cats.Functor
import cats.Traverse
import cats.Applicative
import cats.Foldable
import cats.Eval
import cats.Semigroup
case class Grid[A](values: Seq[Seq[A]]){
  val width: Int = values.head.length 
  val height: Int = values.length
  assert(values.forall(_.lengthCompare(width) == 0))
  def isDefinedAt(x: Int, y: Int): Boolean = {
    (x >= 0 && x < width) && (y >= 0 && y < height)
  }
  def isDefinedAt(n: Int): Boolean = {
    val (x, y) = nToXY(n)
    isDefinedAt(x, y)
  }
  def get(x: Int, y: Int): Option[A] = {
    if (isDefinedAt(x, y))
      Some(apply(x, y))
    else 
      None
  }
  def get(n: Int): Option[A] = {
    val (x, y) = nToXY(n)
    get(x, y)
  }
  def apply(x: Int, y: Int): A = values(y)(x)
  def apply(n: Int) : A = {
    val (x, y) = nToXY(n)
    apply(x, y)
  }
  def extractRow(y: Int): Seq[A] = values(y) 
  def extractColumn(x: Int): Seq[A] = values.transpose.apply(x)
  def rows: Seq[Seq[A]] = values 
  def columns: Seq[Seq[A]] = values.transpose
  private def nToXY(n: Int): (Int, Int) = {
    (n % width, Math.floor(n / width).toInt)
  }
  def updated(x: Int, y: Int)(v: A): Grid[A] = 
    Grid(values.updated(y, values(y).updated(x, v)))
  def updated(n: Int)(v: A): Grid[A] = {
    val (x, y) = nToXY(n)
    updated(x, y)(v)
  }
}
object Grid {
  def apply[A](values: Seq[A], width: Int): Grid[A] = {
    Grid[A](values.grouped(width).toSeq)
  }
}
given gridShow[A](using s: Show[A]): Show[Grid[A]] with {
  def show(t: Grid[A]): String = {
    t.rows.map(it => it.map(s.show _).fold("")(_ + " " + _)).fold[String]("")(_ + "\n" + _) 
  }
}

given gridFunctor: Functor[Grid] with {
  def map[A, B](fa: Grid[A])(f: A => B): Grid[B] = {
    Grid(fa.values.map(_.map(f.apply)))
  }
}

