import cats.*
import cats.implicits.*
import cats.data.*
import scala.io.Source 

val fullTime = 26 
case class ValveRoom(room: String, flowRate: Int, connectsTo: Vector[String]) {
  def value(time: Int) = flowRate * (fullTime - time )
}

type ValveMap = Map[String, ValveRoom]

val start = "AA"

val input = Source.fromResource("day16tst.txt").mkString 

val data = {
  input.trim.linesIterator.map { it => 
    it.trim match {
      case s"Valve $room has flow rate=$n; tunnel$_ lead$_ to valve$_ $rest" => 
        val goodRest = rest.split(",").map(_.trim).toVector 
        ValveRoom(room, n.toInt, goodRest)

      case _ => ???
    }
  }.map(it => (it.room, it)).toMap
}

val startRoom = data(start)
case class State(on: List[String], time: Int, pressure: Int)
case class Position(dest: ValveRoom, progress: Int)
case class ImportantRooms(rooms: ValveMap) {
  val daImportantRooms = importantRooms(rooms, List())
  def edge(p1: ValveRoom, p2: ValveRoom): Eval[Int] = Eval.later {
    astar(p1, p2, _ => 0d, (_, _) => 1d, it => it.connectsTo.map(rooms.apply))
      .map(_.size)
      .getOrElse(fullTime)
  }.memoize
}
def importantRooms(rooms: ValveMap, on: List[String]) = rooms.filter((k, v) => v.flowRate != 0 && !on.contains(k)).values.toVector
def pathsToImportant(rooms: ValveMap, curPos: ValveRoom, on: List[String]) = {
  importantRooms(rooms, on).map { it =>
    (it, graphAStar(curPos.room, it.room, rooms))
  }
}
def shortestTime(rooms: ValveMap, curPos: ValveRoom, on: List[String]): Int = {
  val res = pathsToImportant(rooms, curPos, on).map(_._2).flatten
  if (res.isEmpty)
    fullTime 
  else 
    res.minBy(_.size).size - 1
}
import scala.collection.mutable as mut
def reconstructPath[A](cameFrom: Map[A, A], p: A): List[A] = {

  val totalPath = mut.ListBuffer[A](p)
  var current = p
  while (cameFrom.contains(current)) {
    current = cameFrom(current)
    totalPath.prepend(current)
  }
  totalPath.toList
}
def astar[A](start: A, goal: A, h: A => Double, d: (A, A) => Double, neighbors: A => Iterable[A]): Option[List[A]] = {
  val cameFrom = mut.HashMap[A, A]()

  val gscore = mut.HashMap(start -> 0d)

  val fscore = mut.HashMap(start -> h(start))

  val openSet = mut.PriorityQueue(start)(using Ordering.by(it => fscore.getOrElse(it, Double.PositiveInfinity)).reverse)
  while (openSet.nonEmpty) {
    val current = openSet.dequeue()

    if (current == goal) 
      return Some(reconstructPath(cameFrom.toMap, current))
    for (neighbor <- neighbors(current)) {
      val stinkyGScore = gscore.getOrElse(current, Double.PositiveInfinity) + d(current, neighbor)
      if (stinkyGScore < gscore.getOrElse(neighbor, Double.PositiveInfinity)) {
        cameFrom(neighbor) = current 
        gscore(neighbor) = stinkyGScore
        fscore(neighbor) = stinkyGScore + h(neighbor)
        if (!openSet.exists(_ == neighbor)) 
          openSet.enqueue(neighbor)
      }
    }
  }

  return None
}

def graphAStar(start: String, goal: String, graph: ValveMap): Option[List[String]] = {
  astar(start, goal, _ => 0d, (_, _) => 1d, it => graph(it).connectsTo)
}

case class HappenedNow(pressureAdded: Int, turnedOn: Option[String]) 
def runFirstBit(rooms: ValveMap, pos: Position, state: State): (List[Position], HappenedNow) = {
  if (pos.progress == 0) { 
    val newPressure = pos.dest.value(state.time)
    val res = pathsToImportant(rooms, pos.dest, state.on).map { (room, path) => 
      require(path.isDefined)
      // includes current, and nodes to get there. no minus 1 because of valve turning
      val timeTaken = path.get.size
      val goodTime = state.time + timeTaken
      if (goodTime > fullTime)
        None 
      else 
        Some(Position(room, goodTime - state.time))
    }.flatten 
    (res.toList, HappenedNow(pos.dest.value(state.time), Some(pos.dest.room)))
  } else {
    (List(pos.copy(progress = pos.progress - 1)), HappenedNow(0, None))
  }
}
def run(rooms: ValveMap, pos1: Position, pos2: Position, state: State): Int = {
  val (p1s, HappenedNow(pressure1, turnedOn1)) = runFirstBit(rooms, pos1, state)
  val (p2s, HappenedNow(pressure2, turnedOn2)) = runFirstBit(rooms, pos2, state)
  val goodOn = state.on ++ List(turnedOn1, turnedOn2).flatten 
  val goodPressure = state.pressure + pressure1 + pressure2 
  val newState = state.copy(on = goodOn,pressure = goodPressure, time = state.time + 1)
  if (newState.time == fullTime)
    return newState.pressure 
  val res = for {
    p1 <- p1s 
    p2 <- p2s 
  } yield {
    run(rooms, pos1, pos2, newState)    
  }
  if (res.isEmpty)
    newState.pressure 
  else 
    res.max
}
/*
def resultOfPath(rooms: ValveMap, path: Vector[ValveRoom]) = {
  var pressure = 0 
  var time = 0
  val res = path.sliding(2).map { case Vector(x, y) => 
    time += astar(x.room, y.room, _ => 0d, rooms).get.size
    pressure += y.value(time - 1)
  }.toVector
  if (time > fullTime)
    None 
  else Some((path, pressure))
}
def run2(rooms: ValveMap, curPos: ValveRoom) = {
  val importantRooms = rooms.filter((k, v) => v.flowRate != 0).values.toVector 
  importantRooms.permutations.map(it => resultOfPath(rooms, it)).flatten
}
*/
// resultOfPath(data, Vector(startRoom, data("BB"), data("CC"), data("DD"), data("EE"), data("HH"), data("JJ"))).get._2
// run2(data, startRoom).maxBy(_._2)
