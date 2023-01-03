import cats.*, implicits.* 
import cats.data.* 
import scala.io.Source
import net.bulbyvr.common.*
import org.atnos.eff.* 
import org.atnos.eff.reader.* 
import org.atnos.eff.state.*
import org.atnos.eff.syntax.all.* 

type ReaderBlueprint[A] = Reader[Blueprint, A]
type Day19State[A] = State[DaState, A]
type Day19Members = Fx.fx2[ReaderBlueprint, Day19State]
case class Cost(ore: Int, clay: Int, obsidian: Int)

object Cost {
  def parse(input: String) = {
    var ore = 0
    var clay = 0
    var obsidian = 0 
    input.split(" and ").foreach { it => 
      it.trim match {
        case s"$cost $rtype" => 
          RobotType.parse(rtype) match {
            case RobotType.Ore => ore = cost.toInt 
            case RobotType.Clay => clay = cost.toInt 
            case RobotType.Obsidian => obsidian = cost.toInt
            case RobotType.Geode => !!!
          }
      }
    }
    Cost(ore, clay, obsidian)
  }
}

enum RobotType {
  case Ore, Clay, Obsidian, Geode 
}
object RobotType {
  def parse(input: String) = {
    input match {
      case "ore" => Ore
      case "clay" => Clay 
      case "obsidian" => Obsidian 
      case "geode" => Geode 
      case _ => !!!
    }
  }
}
case class Robot(robotType: RobotType, cost: Cost)

object Robot {
  def parse(input: String) = {
    println(input)
    input.trim match {
      case s"Each $rtype robot costs $cost" => 
        val goodRobotType = RobotType.parse(rtype)
        val goodCost = Cost.parse(cost)
        Robot(goodRobotType, goodCost)
      case _ => !!!
    }
  }
}
case class Blueprint(n: Int, oreRobot: Robot, clayRobot: Robot, obsidianRobot: Robot, geodeRobot: Robot) {
  val robots: List[Robot] = List(oreRobot, clayRobot, obsidianRobot, geodeRobot)
  val lowestOreCost = robots.map { case Robot(robotType, cost) => 
     Option.when(cost.ore != 0)(cost.ore)
  }.flatten.min
  val highestOreCost = robots.map(_.cost.ore).max
  def robot(rType: RobotType) = rType match
    case RobotType.Ore => oreRobot
    case RobotType.Clay => clayRobot 
    case RobotType.Obsidian => obsidianRobot
    case RobotType.Geode => geodeRobot
  
}

object Blueprint {
  def parse(input: String) = {
    input.trim match {
      case s"Blueprint $n: $rest" =>
        val List(ore, clay, obsidian, geode) = rest.trim().split('.').map { robot => 
          Robot.parse(robot.trim)
        }.toList
        Blueprint(n.toInt, ore, clay, obsidian, geode)
      case _ => 
        println(input.trim) 
        !!!
    }
  }
}
case class DaState(
  time: Int, 
  nOre: Int,
  nClay: Int,
  nObsidian: Int, 
  nGeode: Int, 
  nOreRobot: Int,
  nClayRobot: Int, 
  nObsidianRobot: Int,
) {
  def withCost(cost: Cost): Option[DaState] = {
    val newOre = nOre - cost.ore 
    val newClay = nClay - cost.clay 
    val newObsidian = nObsidian - cost.obsidian
    if (newOre >= 0 && newClay >= 0 && newObsidian >= 0) 
      Some(this.copy(nOre = newOre, nClay = newClay, nObsidian = newObsidian))
    else 
      None 
  }
}

val fullTime = 24 
def buildGeodeRobot[E](using member: Day19State |= E): Eff[E, Unit] = 
  for {
    time <- gets[E, DaState, Int](_.time)
    // no + 1 because it is called from buildRobotTurn 
    remainingAfter = fullTime - time
    _ <- modify[E, DaState](it => it.copy(nGeode = it.nGeode + remainingAfter))
  } yield ()
def updateItemsN[E](using member: Day19State |= E): Eff[E, Unit] = 
  for {
    state <- get[E, DaState]
    newOre = state.nOre + state.nOreRobot 
    newClay = state.nClay + state.nClayRobot 
    newObsidian = state.nObsidian + state.nObsidianRobot 
    _ <- put[E, DaState](state.copy(nOre = newOre, nClay = newClay, nObsidian = newObsidian))
  } yield ()
def passiveTurn[E](using member: Day19State |= E): Eff[E, Unit] = 
  for {
    _ <- updateItemsN
    _ <- modify[E, DaState](it => it.copy(time = it.time + 1))
  } yield () 
def buildRobotTurn[E](robotType: RobotType)(using r: ReaderBlueprint |= E, s: Day19State |= E): Eff[E, Unit] = 
  for {
    _ <- updateItemsN
    _ <- modify[E, DaState](it => it.copy(time = it.time + 1))
    blueprint <- ask
    costState <- gets[E, DaState, Option[DaState]](_.withCost(blueprint.robot(robotType).cost))
    _ = if (costState.isEmpty) !!! else ()
    _ <- put(costState.get)
    _ <- robotType match
      case RobotType.Ore => modify[E, DaState](it => it.copy(nOreRobot = it.nOreRobot + 1))
      case RobotType.Clay => modify[E, DaState](it => it.copy(nClayRobot = it.nClayRobot + 1))
      case RobotType.Obsidian => modify[E,DaState](it => it.copy(nObsidianRobot = it.nObsidianRobot + 1))
      case RobotType.Geode => buildGeodeRobot
   
  } yield ()


// Given you have a robot R of type O, that was produced at time t,
// it will produce (24 - t) O 
//
// Geode robots do NOT need to be simulated. The type they produce is NOT used 
// as cost thus the result of the geode robot's work can be immedietly added
// 
// The max values of each type is 23 except for ore which is 24 
// You will have AT LEAST 24 ore at the end
//
// You MUST ignore the first minutes before the lowest price. For test blueprint 1,
// you ignore the first 2 minutes.
//
// For my input nothing costs only 1 of something. 
//
// An ore robot is only worth its price if the time left is greater than its cost. 
// Still likely a ripoff LOL
//
// Buying geode robots as early as possible is IMPORTANT 
//
// Making more of a robot is always useless when the number of robots is greater than or equal to 
// the highest cost of that value
//
// 
val input = Source.fromResource("day19tst.txt").mkString 
val blueprints = input.trim.linesIterator.map(Blueprint.parse _).toVector


