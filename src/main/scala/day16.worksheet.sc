import cats.*
import cats.implicits.*
import cats.data.*
import scala.io.Source 
import scala.collection.mutable as mut 

case class ValveRoom(room: String, flowRate: Int, connectsTo: Vector[String]) {
  def value(time: Int, fullTime: Int) = flowRate * (fullTime - time )
}

type ValveMap = Map[String, ValveRoom]

val start = "AA"

val input = Source.fromResource("day16.txt").mkString 

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

object DistanceCalculator {
  private val memo = mut.HashMap[String, Int]()
  def distance(start: String, end: String): Int = {
    
    val goodStart = Order.min(start, end)
    val goodEnd = Order.max(start, end)
    memo.getOrElseUpdate(s"$goodStart.$goodEnd", astar(start, end, _ => 0d, data).get.size - 1)
  }
}
val startRoom = data(start)
case class State(on: List[String], time: Int, pressure: Int)

def importantRooms(rooms: ValveMap, on: List[String]) = rooms.filter((k, v) => v.flowRate != 0 && !on.contains(k)).values.toVector
def pathsToImportant(rooms: ValveMap, curPos: ValveRoom, on: List[String]) = {
  importantRooms(rooms, on).map { it =>
    (it, astar(curPos.room, it.room, _ => 0d, rooms))
  }
}
import scala.collection.mutable as mut
def reconstructPath(cameFrom: Map[String, String], p: String): List[String] = {

  val totalPath = mut.ListBuffer[String](p)
  var current = p
  while (cameFrom.contains(current)) {
    current = cameFrom(current)
    totalPath.prepend(current)
  }
  totalPath.toList
}
def astar(start: String, goal: String, h: String => Double, graph: ValveMap): Option[List[String]] = {
  val cameFrom = mut.HashMap[String, String]()

  val gscore = mut.HashMap(start -> 0d)

  val fscore = mut.HashMap(start -> h(start))

  val openSet = mut.PriorityQueue(start)(using Ordering.by(it => fscore.getOrElse(it, Double.PositiveInfinity)).reverse)
  while (openSet.nonEmpty) {
    val current = openSet.dequeue()

    if (current == goal) 
      return Some(reconstructPath(cameFrom.toMap, current))
    for (neighbor <- graph(current).connectsTo) {
      val stinkyGScore = gscore.getOrElse(current, Double.PositiveInfinity) + 1
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


def run(rooms: Set[ValveRoom], curPos: ValveRoom, fullTime: Int, time: Int, pressure: Int): Int = {
  val res = rooms.map { room =>
    // includes current, and nodes to get there. no minus 1 because of valve turning
    val timeTaken = DistanceCalculator.distance(curPos.room, room.room) + 1
    val goodTime = time + timeTaken
    Option.when(goodTime <= fullTime) {
      run(rooms - room, room, fullTime, goodTime, pressure + room.value(goodTime, fullTime))
    }
  }.flatten.maxOption 
  res.getOrElse(pressure)
}

val important = importantRooms(data, List()).toSet 
run(important, startRoom, 30, 0, 0)


important.subsets()
.map {  myOpenValves => 
  val elephantValves = important -- myOpenValves
  // lie and say all elephant valves are turned on for your walkthrough
  run(myOpenValves, startRoom, 26, 0, 0)
  + run(elephantValves, startRoom, 26, 0, 0)
}.max
