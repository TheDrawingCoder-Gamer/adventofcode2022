import scala.io.Source
import cats.data.State
import cats.data.*
import cats.implicits.* 
import cats.Eval
val input = Source.fromResource("day11tst.txt").mkString

case class Monkey(n: Int, items: Seq[Long], op: Long => Long, divisor: Int, throwTrue: Int, throwFalse: Int)

def removeFirst[A](xs: Seq[A])(n: A): Seq[A] = {
  val splitAt = xs.indexWhere(_ == n)
  val (l, r) = xs.splitAt(splitAt)
  l ++ r.drop(1)
}
def parseOp(op: String): Long => Long = 
  op.trim match {
    case s"$left $middle $right" => 
      n => 
        val l = if (left == "old") n else left.toLong 
        val r = if (right == "old") n else right.toLong
        middle match {
          case "+" => l + r 
          case "*" => l * r 
          case _ => ???
        }
  }
val divisibleByRegex = raw"divisible by ([0-9]+)".r
val throwToMonkey = raw"throw to monkey ([0-9]+)".r
val monkeys = input.split("\n\n").map { it => 
  val xs = it.linesIterator.toVector
  val monkey = xs(0).drop("Monkey ".length).dropRight(1).toInt
  val items = xs(1).trim.drop("Starting items:".length).split(',').map(_.trim.toLong).toSeq
  val op = parseOp(xs(2).trim.drop("Operation: new = ".length))
  val divisor = divisibleByRegex.findFirstMatchIn(xs(3)).map(_.group(1).toInt).get
  val ifTrue = throwToMonkey.findFirstMatchIn(xs(4)).map(_.group(1).toInt).get
  val ifFalse = throwToMonkey.findFirstMatchIn(xs(5)).map(_.group(1).toInt).get
  Monkey(monkey, items, op, divisor, ifTrue, ifFalse)
  
}.toVector
val sanityLoss = 1
val modulus = monkeys.map(_.divisor).product
val rounds = 10000
def oneToss(i : Int): State[Vector[Monkey], Int] = State { monkes => 
  val m = monkes(i)
  val inspected = m.items.size 
  println(s"before monkes: ${monkes.map(_.items)}")
  val (tt, tf) = m.items.partitionMap { item =>
    val res = (m.op(item) / sanityLoss) % modulus
    if (res % m.divisor == 0) Left(res) else Right(res)
  }
  println(tt)
  println(tf)
  val trueMonke = monkes(m.throwTrue)
  val falseMonke = monkes(m.throwFalse)
  val newMonkes: Vector[Monkey] = 
    monkes.updated(m.n, m.copy(items = Seq())).updated(m.throwTrue, trueMonke.copy(items = trueMonke.items ++ tt)).updated(m.throwFalse, falseMonke.copy(items = falseMonke.items ++ tf))
  println(s"after monkes: ${newMonkes.map(_.items)}")
  (newMonkes, inspected)
}
val monkeToss: State[Vector[Monkey], Vector[Int]] = 
  for {
    state <- State.get[Vector[Monkey]]   
    res <- state.traverse(m => oneToss(m.n))
  } yield res



val res = monkeToss.replicateA(rounds).runA(monkeys).value.transpose.map(_.sum)
res.sorted.takeRight(2).map(_.toLong).product



