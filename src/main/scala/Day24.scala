import net.bulbyvr.common.*
import GridAxisSystem.* 
import scala.collection.mutable as mut 
/** 
 * This class is NOT thread safe!
 */
object Day24 {

  sealed trait BlizzardMapPoint
  case class BlizzardSpot(blizzards: List[Direction2D]) extends BlizzardMapPoint 
  case object Wall extends BlizzardMapPoint
  case object Empty extends BlizzardMapPoint  

  case class TimeLocation(time: Int, loc: Vec2i)
  
  type BlizzardMap = Grid[BlizzardMapPoint]

  def nextMap(blizzards: BlizzardMap, time: Int): BlizzardMap = {
    if (memo.contains(time))
      return memo(time)
    val daThingie = 
      (for {
        yy <- 0 until blizzards.height 
        xx <- 0 until blizzards.width 
      } yield { 
        val spot = blizzards(xx, yy)
        spot match
          case BlizzardSpot(blizzards) => 
            Some(blizzards.map(it => (Vec2i(xx, yy), it)))
          case _ => None 
        
      }).flatten.flatten 
    val newPoses = 
      daThingie.map { (pos, dir) => 
        val newPos = pos.offset(dir) 
        val goodPos = 
          if (blizzards.getOrElse(newPos, Wall) == Wall) 
            dir match
              case Direction2D.Up => 
                newPos.copy(y = blizzards.extractColumn(pos.x).lastIndexWhere(_ != Wall))
              case Direction2D.Down =>
                newPos.copy(y = blizzards.extractColumn(pos.x).indexWhere(_ != Wall))
              case Direction2D.Left =>
                newPos.copy(x = blizzards.extractRow(pos.y).lastIndexWhere(_ != Wall))

              case Direction2D.Right => 
                newPos.copy(x = blizzards.extractRow(pos.y).indexWhere(_ != Wall))
          else 
            newPos 
        (goodPos, dir)
      }
    val daMap = newPoses.groupMap(_._1)(_._2).mapValues(_.toList)
    var newGrid = Grid(blizzards.flatten.map {
      case BlizzardSpot(blizzards) => Empty 
      case self => self 
    }, blizzards.width)
    for {
      (k, v) <- daMap 
    } {
      val cur = newGrid(k)
      val newSpot = cur match
        case BlizzardSpot(blizzards) => BlizzardSpot(blizzards ++ v)
        case Wall => ??? 
        case Empty => BlizzardSpot(v)
      
      newGrid = newGrid.updated(k)(newSpot)
    }
    memo.put(time, newGrid) 
    newGrid
  }

  def neighbors(blizzards: BlizzardMap, pos: TimeLocation) = {
    val newMap = nextMap(blizzards, pos.time + 1)
    val daDirs = (for {
      p <- Direction2D.values.map(it => pos.loc.offset(it)).toList.prepended(pos.loc)
    } yield {
      newMap.get(p).flatMap {
        case Empty => Some(pos.copy(loc = p, time = pos.time + 1)) 
        case _ => None 
      }
      
    }).flatten
    
    daDirs

  }
  val memo = mut.HashMap[Int, BlizzardMap]()
  def daAStar(start: Vec2i, goal: Vec2i) = astarGeneric[TimeLocation](TimeLocation(0, start), it => it.loc == goal, { it => 
      it.loc `taxiDistance` goal
  },
  (_, _) => 1d, it => neighbors(memo(it.time), it))

  def parse(input: String): BlizzardMap = {
    val points = input.linesIterator.map {
      _.map {
        case '.' => Empty 
        case '#' => Wall 
        case '^' => BlizzardSpot(List(Direction2D.Up))
        case 'v' => BlizzardSpot(List(Direction2D.Down))
        case '<' => BlizzardSpot(List(Direction2D.Left))
        case '>' => BlizzardSpot(List(Direction2D.Right))
        case _ => ???
      }
    }
    Grid(points)
  }

  def run(input: String) = {
    memo.clear()
    val data = parse(input)
   
    memo.put(0, data)
    val start = Vec2i(data.rows.head.indexOf(Empty), 0)
    val end = Vec2i(data.rows.last.indexOf(Empty), data.height - 1)
    daAStar(start, end).get.size - 1

  }
}
