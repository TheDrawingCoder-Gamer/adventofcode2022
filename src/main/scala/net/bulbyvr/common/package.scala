package net.bulbyvr.common

import scala.collection.mutable as mut 
def !!! : Nothing = throw new IllegalArgumentException("Unexpected")

def reconstructPath[A](cameFrom: Map[A, A], p: A): List[A] = {

  val totalPath = mut.ListBuffer[A](p)
  var current = p
  while (cameFrom.contains(current)) {
    current = cameFrom(current)
    totalPath.prepend(current)
  }
  totalPath.toList
}

/** 
 * A generalized astar
 * @tparam A the element type in the graph
 * @param start the start point
 * @param isGoal test for end point
 * @param h the heuristic function. Represents how long it is assumed to take to go from the point supplied to the end.
 * * For a derived astar to be admissible the heuristic function must be admissable. Put simply, h must NEVER return a result greater 
 * than the actual value. 
 * @param d The edge function. Returns how much it costs to take an edge. Is a double to represent edges that aren't existant (use
 * [[scala.Double.PositiveInfinity]] for this case). If the edge weight depends on direction, the first parameter represents the start node 
 * and the second represents the end node.
 * @param neighbors Returns all nodes that connect to the passed node.
 * @returns An Option containing the shortest path taken to reach the end node. This path includes the start and end node.
 */
def astarGeneric[A](start: A, isGoal: A => Boolean, h: A => Double, d: (A, A) => Double, neighbors: A => IterableOnce[A]): Option[List[A]] = {
  val cameFrom = mut.HashMap[A, A]()

  val gscore = mut.HashMap(start -> 0d)

  val fscore = mut.HashMap(start -> h(start))

  val openSet = mut.PriorityQueue(start)(using Ordering.by(it => fscore.getOrElse(it, Double.PositiveInfinity)).reverse)
  while (openSet.nonEmpty) {
    val current = openSet.dequeue()

    if (isGoal(current)) 
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
/**
 * Simplified generialized AStar
 * Removes isGoal in favor of goal, which does `_ == goal`. 
 * See [[astarGeneric]]
 */
def astar[A](start: A, goal: A, h: A => Double, d: (A, A) => Double, neighbors: A => IterableOnce[A]): Option[List[A]] = astarGeneric(start, _ == goal, h, d, neighbors)
