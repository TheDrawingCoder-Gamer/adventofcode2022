import scala.jdk.StreamConverters.*
object Day2 {
  def parse(input : String) : List[Throw] = {
    input.lines().map { line =>
      val parts = line.split(' ').take(2)
      // 65 == A
      // 88 = X
      val opponent = RPS.fromOrdinal(parts(0).charAt(0) - 'A')
      val player = RPS.fromOrdinal(parts(1).charAt(0) - 'X')
      Throw(opponent, player)
    }.toScala(List)
  }
  def parseDay2(input : String) : List[Throw] = {
    input.lines().map { line =>
      val parts = line.split(' ').take(2)
      val opponent = RPS.fromOrdinal(parts(0).charAt(0) - 'A')
      val player = RPSResult.fromOrdinal(parts(1).charAt(0) - 'X')
      Strategy(opponent, player).toThrow
    }.toScala(List)
  }
  def run(input : String) : Int = {
    val data = parse(input)
    data.map(_.score).sum
  }
  def runP2(input : String) : Int = {
    val data = parseDay2(input)
    data.map(_.score).sum
  }
}
enum RPSResult {
  case Loss, Draw, Win
  def score : Int = {
    this match {
      case Loss => 0
      case Draw => 3
      case Win => 6
    }
  }
}
enum RPS {
  case Rock, Paper, Scissors

  def beatsOther(that : RPS) : Boolean = {
    this match {
      case Rock => that == Scissors
      case Paper => that == Rock
      case Scissors => that == Paper
    }
  }
  def beats : RPS = {
    this match {
      case Rock => Scissors
      case Paper => Rock
      case Scissors => Paper
    }
  }
  def beatenBy : RPS = {
    this match {
      case Rock => Paper
      case Paper => Scissors
      case Scissors => Rock
    }
  }
  def result(that : RPS) : RPSResult = {
    if (this.beatsOther(that)) {
      RPSResult.Win
    } else if (that.beatsOther(this)) {
      RPSResult.Loss
    } else {
      RPSResult.Draw
    }
  }
  def score: Int = ordinal + 1
}
case class Throw(opponent : RPS, player : RPS) {
  def score : Int = {
    // real
    val shapeScore = player.score
    val result = player.result(opponent).score
    result + shapeScore
  }
}
case class Strategy(opponent : RPS, player : RPSResult) {
  def toThrow : Throw = {
    val newPlayer = player match {
      case RPSResult.Win => opponent.beatenBy
      case RPSResult.Draw => opponent
      case RPSResult.Loss => opponent.beats
    }
    Throw(opponent, newPlayer)
  }
}