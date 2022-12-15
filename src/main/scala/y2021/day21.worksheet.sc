import cats.* 
import cats.implicits.*
import cats.syntax.all.*
import cats.data.{State, Reader}
import scala.io.Source

trait Dice {
  def next: Dice
  def value: Int
}

case class DeterministicDice(seed: Int, size: Int) {
  def next: DeterministicDice = this.copy(seed = (seed + 1) % size)
}
case class Player(score: Int, space: Int) {
  def move(n: Int): Player = {
    val daSpace = (space + n) % 10
    val good = if (daSpace == 0) 10 else daSpace
    this.copy(score = score + good, space = good)
  }
}
case class DaState(dice: DeterministicDice, nrolls: Int, p1: Player, p2: Player)
val nextInt: State[DeterministicDice, Int] = State { it => 
  val newOne = it.next 
  (newOne, it.seed)
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
  } yield res >= 1000
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
val input = Source.fromResource("2021/day21.txt").mkString 
val List(player1, player2) = input.linesIterator.map { it => 
  Player(0, it.dropWhile(_ != ':').drop(1).trim.toInt )
}.toList



val (state, winner) = fullTurn.untilDefinedM.run(DaState(DeterministicDice(0, 100), 0, player1, player2)).value
state.p2
winner match
  case 1 => 
    state.p2.score * state.nrolls 
  case 2 => 
    state.p1.score * state.nrolls
  case _ => 0

