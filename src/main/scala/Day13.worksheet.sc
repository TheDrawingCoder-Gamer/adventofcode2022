import scala.io.Source 
import cats.*
import cats.parse.* 
import cats.implicits.*
import cats.syntax.all.*


sealed trait Packet extends Ordered[Packet] {
  def compare(that: Packet): Int = {
    (this, that) match
      case (NestedPacket(left), NestedPacket(right)) =>
        lazy val goodLength = left.length.compare(right.length)
      
        left.zip(right).collectFirst(Function.unlift { (l, r) => 
          
          val res = l.compare(r)
          if (res != 0) 
            Some(res)
          else None 

        } ).getOrElse(goodLength)
      case (LeafPacket(left), LeafPacket(right)) => left `compare` right
      case (l @ LeafPacket(left), r @ NestedPacket(right)) => 
        NestedPacket(List(l)) `compare` r 
      case (left @ NestedPacket(_), r @ LeafPacket(_)) => 
        left `compare` NestedPacket(List(r))
    
  }
  override def equals(x: Any): Boolean = 
    x match
     case p: Packet => 
       this.compare(p) == 0
     case _ => false
    
} 

given orderPacket: Order[Packet] = Order.fromOrdering(using Ordering.ordered)
case class NestedPacket(packets: List[Packet]) extends Packet 
case class LeafPacket(n: Int) extends Packet 
case class PacketPair(top: Packet, bottom: Packet) {
  def compared: Comparison = top `comparison` bottom
    
}
val divider1 = NestedPacket(List(NestedPacket(List(LeafPacket(2))))) 
val divider2 = NestedPacket(List(NestedPacket(List(LeafPacket(6))))) 

val leafParser: Parser[LeafPacket] = Numbers.bigInt.map(it => LeafPacket(it.toInt))
val packetParser: Parser[Packet] = Parser.recursive[Packet] { recur => 
  leafParser.orElse(Parser.string("[]").as(NestedPacket(List())))
  .orElse(recur.repSep(Parser.char(',')).between(Parser.char('['), Parser.char(']')).map(it => NestedPacket(it.toList)))
}
val input = Source.fromResource("day13.txt").mkString

val data = input.linesIterator.filterNot(_.isEmpty).map { it =>
  packetParser.parseAll(it).getOrElse(throw IllegalArgumentException())
}.toVector
// val dataP1 = data.grouped(2).map { case Vector(l, r) => PacketPair(l, r) }.toVector 
// val compared = dataP1.map(it => (it, it.compared))
val dataP2 = data.prependedAll(Seq(divider1, divider2)).sorted
(dataP2.indexOf(divider1) + 1) * (dataP2.indexOf(divider2) + 1)
