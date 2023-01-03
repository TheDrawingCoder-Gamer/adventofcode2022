import cats.*, implicits.*
import cats.syntax.all.*
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

val input = Source.fromResource("day20tst.txt").mkString
val data = input.linesIterator.map(_.toInt).to(ArrayBuffer)

// assuming all numbers are unique 
val numbers = data.toVector
val dataSize = data.size

// If something moves backward into position 0 it is instead at 
// dataSize - 1

Day20Helper.mixNumbers(data)

val zeroIdx = data.indexOf(0)

val x = Day20Helper.mix(zeroIdx, 1000, dataSize)
val y = Day20Helper.mix(zeroIdx, 2000, dataSize)
val z = Day20Helper.mix(zeroIdx, 3000, dataSize)
x + y + z
