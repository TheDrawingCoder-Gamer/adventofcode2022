
import scala.io.Source
import cats.implicits.*
import cats.syntax.all.*
import cats.*
import net.bulbyvr.common.{Grid, given}
val input = Source.fromResource("2021/day20.txt").mkString

val List(t: String, b: String) = input.trim.split("\n\n").toList : @unchecked
val algo = t.trim.map(_ == '#').toVector
val image = Image(Grid(b.linesIterator.map(_.trim.map(_ == '#')).toVector), false)


case class Image(grid: Grid[Boolean], oobPixel: Boolean) {
  def apply(x: Int, y: Int): Boolean = 
    grid.get(x, y).getOrElse(oobPixel)
  def expand(n: Int): Image = 
    Image(grid.expand(oobPixel)(n), oobPixel)
  def height = grid.height 
  def width = grid.width 
  def valuesAround(x: Int, y: Int): Grid[Boolean] = grid.valuesAround(oobPixel)(x, y)
  def flatten: Vector[Boolean] = grid.flatten
  def countLitPixels: Int = grid.values.view.flatten.count(identity)
}
@annotation.tailrec
final def boolsToInt(bs: List[Boolean], accum: Int = 0): Int = {
  bs match {
    case Nil => accum 
    case true :: next => boolsToInt(next, (accum << 1) | 1)
    case false :: next => boolsToInt(next, (accum << 1) | 0)
  }
}
def convolute(img: Image): Image = {
  val newImg = Grid(for {
    y <- -1 to img.height  
    x <- -1 to img.width  
  } yield {
    val values = img.valuesAround(x, y)
    algo(boolsToInt(values.flatten.toList))
  }, img.width + 2)
  val daIndex = if img.oobPixel then 511 else 0
  Image(newImg, algo(daIndex))
}

def showGrid(grid: Grid[Boolean]): String = {
  grid.values.map { it => 
    it.map(if (_) '#' else '.')
  }.foldLeft("")((accum, data) => accum + "\n" + data.mkString("", "", ""))
}
// val res = convolute.andThen(convolute).apply(image)
val res = LazyList.iterate(image)(convolute _)(50)
res.countLitPixels
