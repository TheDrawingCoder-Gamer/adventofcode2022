import scala.io.Source
import scala.collection.mutable as mut
import scala.math.BigInt
class Monkey(val n: Int, val items: mut.ListBuffer[Long], val op: MonkeyOp, val divisor: Int, val throwTrue: Int, val throwFalse: Int, var inspected: Long = 0) {
  /*
  def inspect(monkeys: Vector[Monkey]): Vector[Long] = {
    items.foreach {it => 
      val res = op(it)
      val res2 = (res / sanityLoss) % divisor
      val bool = res2 == 0
      val thingie = if (bool) throwTrue else throwFalse 

      monkeys(thingie).items += res2 

    
    }
    val res = items.toVector 
    items.clear()
    res
  }
  */
}

enum MonkeyOp(l: Option[Long], r: Option[Long]) {
  case Addition(l: Option[Long], r: Option[Long]) extends MonkeyOp(l, r)
  case Multiply(l: Option[Long], r: Option[Long]) extends MonkeyOp(l, r)
  
  def apply(worry: Long): Long = {
    val gl = l.getOrElse(worry)
    val gr = r.getOrElse(worry)
    this match {
      case Addition(_, _) => gl + gr 
      case Multiply(_, _) => gl * gr
    } 
  }
  
}
val input = Source.fromResource("day11.txt").mkString
val monkeyItems: mut.ListBuffer[Vector[Long]] = mut.ListBuffer()

val multiplyRegex = raw"([a-z0-9]+) \* ([a-z0-9]+)".r
val additionRegex = raw"([a-z0-9]+) \+ ([a-z0-9]+)".r
val divisibleByRegex = raw"divisible by ([0-9]+)".r
val throwToMonkey = raw"throw to monkey ([0-9]+)".r
val monkeys = input.split("\n\n").map { it => 
  val xs = it.linesIterator.toVector
  val monkey = xs(0).drop("Monkey ".length).dropRight(1).toInt
  val items = xs(1).trim.drop("Starting items:".length).split(',').map(_.trim.toLong).toSeq
  val op = xs(2).trim.drop("Operation: new = ".length).trim
  val goodOp = 
    multiplyRegex.findFirstMatchIn(op).map(it => MonkeyOp.Multiply(it.group(1).toLongOption, it.group(2).toLongOption))
    .orElse(additionRegex.findFirstMatchIn(op).map(it => MonkeyOp.Addition(it.group(1).toLongOption, it.group(2).toLongOption)))
  val test = divisibleByRegex.findFirstMatchIn(xs(3)).map(_.group(1).toInt).get
  val ifTrue = throwToMonkey.findFirstMatchIn(xs(4)).map(_.group(1).toInt).get
  val ifFalse = throwToMonkey.findFirstMatchIn(xs(5)).map(_.group(1).toInt).get
  Monkey(monkey, items.to(mut.ListBuffer), goodOp.get, test, ifTrue, ifFalse)
  
}.toVector

val modulus = monkeys.map(_.divisor).product 
val sanityLoss = 1 
for {
  _ <- 1 to 10000
  m <- monkeys   
  } {
    for (i <- m.items) {
      val res = m.op(i)
      val res2 = (res / sanityLoss) % modulus 
      val bool = res2 % m.divisor == 0
      val thingie = if (bool) m.throwTrue else m.throwFalse 
      m.inspected += 1
      monkeys(thingie).items.prepend(res2)
       
    }
    m.items.clear()
}
val monkeySums = monkeys.map(_.inspected)
val List(l: Long, r: Long) = monkeySums.sorted.takeRight(2).toList : @unchecked
l * r

