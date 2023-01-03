import cats.* 
import cats.implicits.*
import cats.syntax.all.*
import cats.data.{State, Reader}
import scala.io.Source
/*
trait Dice {
  def next: Dice
  def value: Int
}

case class StaticDice(value: Int) extends Dice {
  def next = this 
}
lazy val diracDiceFullTurn: State[DaState, 
lazy val runDiracDice: Reader[DaState, (BigInt, BigInt)] = Reader { state =>
  val (x, xw) = fullTurn.run(state.copy(dice = StaticDice(0))).value
  val (y, yw) = fullTurn.run(state.copy(dice = StaticDice(1))).value 
  val (z, zw) = fullTurn.run(state.copy(dice = StaticDice(2))).value 
  val (x1, x2) = xw.map(it => if (it == 1) (BigInt(1), BigInt(0)) else (BigInt(0), BigInt(1))).getOrElse(runDiracDice(x))
  val (y1, y2) = yw.map(it => if (it == 1) (BigInt(1), BigInt(0)) else (BigInt(0), BigInt(1))).getOrElse(runDiracDice(y))
  val (z1, z2) = zw.map(it => if (it == 1) (BigInt(1), BigInt(0)) else (BigInt(0), BigInt(1))).getOrElse(runDiracDice(z))
  (x1 + y1 + z1, x2 + y2 + z2)
}

case class DaState(dice: Dice, nrolls: Int, p1: Player, p2: Player)
val nextInt: State[Dice, Int] = State { it => 
  val newOne = it.next 
  (newOne, it.value)
}

val nextIntIdx1 = nextInt.map(_ + 1)
val rollState: State[DaState, Int] = 
  State.get[DaState].flatMap { it => 
    for {
      res <- nextIntIdx1.dimap[DaState, DaState](_.dice)( d => it.copy(dice = d))
      _ <- State.modify[DaState](i => i.copy(nrolls = i.nrolls + 1))
    } yield res
  }
val takeTurn = 
  for {
    x <- rollState
    y <- rollState
    z <- rollState 
    
  } yield x + y + z
def playerTurn(accessor: DaState => Player, setter: (DaState, Player) => DaState): State[DaState, Boolean] = 
  for {
    score <- takeTurn 
    _ <- State.modify[DaState](state => setter(state, accessor(state).move(score)))
    res <- State.inspect[DaState, Int](accessor.andThen(_.score))
  } yield res >= 21
val p1Turn = playerTurn(_.p1, (state, player) => state.copy(p1 = player))
val p2Turn = playerTurn(_.p2, (state, player) => state.copy(p2 = player))
val fullTurn = for {
  r1 <- p1Turn 
  r2 <- 
    if (r1)
      State.pure(Some(1))
    else 
      p2Turn.map(if (_) Some(2) else None)

} yield r2



runDiracDice(DaState(StaticDice(0), 0, player1, player2))
*/ 
case class SinfulState(dice: Int, nrolls: Int, p1: Player, p2: Player)
case class Player(score: Int, space: Int) {
  def move(n: Int): Player = {
    val daSpace = (space + n) % 10
    val good = if (daSpace == 0) 10 else daSpace
    this.copy(score = score + good, space = good)
  }
}
def split[A](action: Reader[SinfulState, List[(SinfulState, A)]]): Reader[SinfulState, List[(SinfulState, A)]] = {

  for {
    x <- Reader.local[List[(SinfulState, A)], SinfulState](_.copy(dice = 1))(action)
    y <- Reader.local[List[(SinfulState, A)], SinfulState](_.copy(dice = 2))(action)
    z <- Reader.local[List[(SinfulState, A)], SinfulState](_.copy(dice = 3))(action)
  } yield x ++ y ++ z
} 
val dieCombos: List[(Int, Long)] = {
  val possibleRolls = 
    (for {
      x <- 1 to 3
      y <- 1 to 3 
      z <- 1 to 3
    } yield x + y + z).toList 
  possibleRolls.groupMapReduce(identity)(_ => 1L)(_ + _).toList
}
val dieMap = dieCombos.toMap
def playerTurn(accessor: SinfulState => Player, setter: (SinfulState, Player) => SinfulState): Reader[SinfulState, List[(SinfulState, Option[BigInt])]] = Reader { state =>
  

  dieCombos.map { (it, v) =>
    val newPlayer = accessor(state).move(it)
    (setter(state, newPlayer), Option.when(newPlayer.score >= 21)(BigInt(v)))

  }

}

val player1Turn = playerTurn(_.p1, (s, p) => s.copy(p1 = p))
val player2Turn = playerTurn(_.p2, (s, p) => s.copy(p2 = p))

val fullTurn: Reader[SinfulState, (List[SinfulState], (BigInt, BigInt))] = Reader { state => 
  val p1 = player1Turn(state)
  val p2 = p1.filter((s, p) => p.isEmpty).map((s, _) => s).flatMap(player2Turn.apply)
  (p2.filter((_, p) => p.isEmpty).map((s, _) => s), (
    p1.filter((_, p) => p.isDefined).map((_, p) => p.get).sum, 
    p2.filter((_, p) => p.isDefined).map((_, p) => p.get).sum
    ))
}

val input = Source.fromResource("2021/day21tst.txt").mkString 
val List(player1, player2) = input.linesIterator.map { it => 
  Player(0, it.dropWhile(_ != ':').drop(1).trim.toInt )
}.toList
def runDiracDice(state: (BigInt, BigInt) = {
  var p1Wins = BigInt(0)
  var p2Wins = BigInt(0)
  var states = List[SinfulState](SinfulState(0, 0, player1, player2))
  while (states.nonEmpty) {
    val res = states.map(fullTurn.apply)
    res.map { case (s, (p1, p2)) => 
      p1Wins += p1 
      p2Wins += p2 
    }
    states = res.flatMap { case (s, _) => s }
  }
  (p1Wins, p2Wins)
}
runDiracDice
