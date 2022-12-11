import scala.collection.mutable as mut 
import scala.jdk.StreamConverters.*
import scala.io.Source

def parsePalletes(input: String): Palletes = {
  Palletes(input.lines().map(parseRow _).toScala(List).reverse.transpose[Option[Crate]].map(it => mut.ListBuffer.from(it.filter(_.isDefined).map(_.get))))
}
def parseRow(input: String): List[Option[Crate]] = {
  var good = input
  var temp = true 
  Iterator.continually {
    val crate = good.take(3)
    good = good.drop(4)
    if (crate.startsWith("["))
      Some(crate(1))
    else 
      None 
  }.takeWhile{ _ => 
    val cur = temp
    temp = good.nonEmpty
    cur
  }.toList
}

def parse(input : String) : (Palletes, List[CraneMove]) = {
  val l :: (r :: _) = input.split("\n\n").toList : @unchecked
  (parsePalletes(l), parseMoves(r))
}



val moveRegex = raw"move (\d+) from (\d+) to (\d+)".r
def parseMoves(input: String): List[CraneMove] = {
  input.lines().map { move => 
    val daMatch = moveRegex.findFirstMatchIn(move).get
    CraneMove(daMatch.group(1).toInt, daMatch.group(2).toInt - 1, daMatch.group(3).toInt - 1)
  }.toScala(List)
}
type Crate = Char 
// Ordered from bottom to top
type Pallete = mut.ListBuffer[Crate] 


class Palletes(val palletes : List[Pallete]) {
  private def moveWith(n : Int, from : Int, to : Int)(extractor : Pallete => mut.ListBuffer[Crate]) : Unit = {
    val crates = extractor(palletes(from))
    palletes(from).dropRightInPlace(n)
    palletes(to) ++= crates
  }
  def moves(n : Int, from : Int, to : Int) : Unit = {
    moveWith(n, from, to)(_.takeRight(n).reverse)
  }
  def movesMany(n : Int, from : Int, to : Int) : Unit = moveWith(n, from, to)(_.takeRight(n))
  def perform(craneMove : CraneMove) : Unit = {
    moves(craneMove.n, craneMove.from, craneMove.to) 
  }
  def performMany(craneMove : CraneMove) : Unit = {
    movesMany(craneMove.n, craneMove.from, craneMove.to)
  }
  def performs(craneMoves : Seq[CraneMove]) : Unit = {
    craneMoves.foreach(perform _)
  }
  def performsMany(craneMoves : Seq[CraneMove]) : Unit = {
    craneMoves.foreach(performMany _)
  }
  
}

case class CraneMove(n : Int, from : Int, to : Int)

val input = Source.fromResource("day5.txt").mkString 
val (palletes, moves) = parse(input)
palletes.performsMany(moves)
palletes.palletes.map(_.last)

