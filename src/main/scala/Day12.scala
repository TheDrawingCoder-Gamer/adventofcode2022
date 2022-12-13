import cats.implicits.*
import cats.*
import cats.syntax.all.*
import net.bulbyvr.common.Grid 
import net.bulbyvr.common.{Direction2D, Vec2i}
import cats.data.State
import scala.jdk.StreamConverters.*
import scala.io.Source
import cats.collections.*
import cats.collections.syntax.all.*
object Day12 {
  case class MountainMap(grid: Grid[Byte]) {
    def verticies: Vector[Vec2i] = 
      (for {
        xx <- 0 until grid.width
        yy <- 0 until grid.height 
      } yield Vec2i(xx, yy)).toVector
    def neighbors(x: Int, y: Int): Iterable[Byte] = 
      neighbors(Vec2i(x, y))
    def neighborVerts(p: Vec2i): Iterable[Vec2i] = 
      Direction2D.values.map(it => p.offset(it)).filter(it => hasEdge(p, it))
    def neighbors(p: Vec2i): Iterable[Byte] = neighborVerts(p).map[Byte](p => grid.get(p).getOrElse(28)).toSeq 
    // travelling from u to v
    def hasEdge(u: Vec2i, v: Vec2i): Boolean = {
      if (!grid.isDefinedAt(u.x, u.y) || !grid.isDefinedAt(v.x, v.y)) 
        false
      else {
        val uv = grid(u)
        val vv = grid(v)
        vv - uv <= 1
      }
    }
    def size = grid.width * grid.height
    def filterIndex(f: (Vec2i, Byte) => Boolean): Vector[(Vec2i, Byte)] = {
      (for {
        vert <- verticies 
      } yield (vert, grid(vert))).filter(f.tupled)
    }
  }

  def manhattanDistance(v1: Vec2i, v2: Vec2i) = Math.abs(v1.x - v2.x) + Math.abs(v1.y - v2.y)

  /*
  case class DijstraState(dist: Map[Vec2i, Int], queue: List[Vec2i], prev: Map[Vec2i, Vec2i])
  def prepVertex(graph: MountainMap, source: Vec2i)(p: Vec2i): State[DaState, Unit] = State { case DaState(dist, queue, prev) => 
    (DaState(dist.updated(p, 255), queue.prepended(p), prev), ())
  }
  def step(graph: MountainMap, source: Vec2i): State[DaState, Unit] = {
    for {
      state <- State.get[DaState]
      DaState(dist, queue, prev) = state
      u = queue.minBy(dist.apply)
      newQ = queue.filterNot(_ == u)
      _ <- State.set[DaState](DaState(dist, newQ, prev))
      _ <- 
        graph.neighborVerts(u).filter(newQ.contains).toVector.traverse { v =>
          val hasEdge = graph.hasEdge(u, v)
          val alt = dist(u) 
          if (hasEdge) {
            State.modify[DaState] { case DaState(dist, queue, prev) => DaState(dist.updated(v, 1), queue, prev.updated(v, u))}
          } else {
            State.inspect[DaState, Unit](_ => ())
          }
        }
    } yield ()

  } 
  def dijstrasAlgorithim(graph: MountainMap, start: Vec2i): DaState = {
    val state =
      for {
        _ <- graph.verticies.traverse(prepVertex(graph, start).apply)
        _ <- State.modify[DaState](state => state.copy(dist = state.dist.updated(start, 0)))
        size <- State.inspect[DaState, Int](state => state.queue.size) 
        _ <- step(graph, start).replicateA_(size) 
      } yield ()
    state.runS(DaState(Map(), List(), Map())).value
    
  } 
  */
  import collection.mutable as mut 
  def reconstructPath(cameFrom: Map[Vec2i, Vec2i], p: Vec2i): List[Vec2i] = {

    val totalPath = mut.ListBuffer[Vec2i](p)
    var current = p
    while (cameFrom.contains(current)) {
      current = cameFrom(current)
      totalPath.prepend(current)
    }
    totalPath.toList
  }
  def neighbors(p: Vec2i): List[Vec2i] = 
    Direction2D.values.map(it => p.offset(it)).toList
  def astar(starts: Vector[Vec2i], goal: Vec2i, h: Vec2i => Double, graph: MountainMap): Option[List[Vec2i]] = {
    val cameFrom = mut.HashMap[Vec2i, Vec2i]()

    val gscore = mut.HashMap.from(starts.map(_ -> 0d))

    val fscore = mut.HashMap.from(starts.map(it => it -> h(it)))

    val openSet = mut.PriorityQueue.from(starts)(using Ordering.by(it => fscore.getOrElse(it, Double.PositiveInfinity)).reverse)
    while (openSet.nonEmpty) {
      val current = openSet.dequeue()

      if (current == goal) 
        return Some(reconstructPath(cameFrom.toMap, current))
      for (neighbor <- graph.neighborVerts(current)) {
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

  case class AStarState(cameFrom: Map[Vec2i, Vec2i], gscore: Map[Vec2i, Int], fscore: Map[Vec2i, Int], queue: Heap[Vec2i])

  def updateNewScore(stinkyScore: Int, neighbor: Vec2i, current: Vec2i, h: Vec2i => Int): State[AStarState, Unit] = State { case AStarState(cameFrom, gscore, fscore, queue) =>
    val newfscore = fscore.updated(neighbor, stinkyScore + h(neighbor))
    val goodOrder = Order.reverse(Order.by(it => newfscore(it)))
    val newQueue = 
      if (!queue.contains(neighbor)(using goodOrder)) 
        queue.add(neighbor)(using goodOrder)
      else 
        queue
    (AStarState(cameFrom.updated(neighbor, current), gscore.updated(neighbor, stinkyScore), newfscore, newQueue), ())
  }
  def astarMain(goal: Vec2i, h: Vec2i => Int, graph: MountainMap): State[AStarState, Option[List[Vec2i]]] = {
    for {
      current <- State.inspect[AStarState, Option[Vec2i]](_.queue.getMin) 
      res <- current.traverse { cur => 
        if (cur == goal)
          State.inspect[AStarState, Option[List[Vec2i]]](it => Some(reconstructPath(it.cameFrom, cur)))
        else for {
            _ <- graph.neighborVerts(cur).toVector.traverse { neighbor => 
            for {
              stinkyGscore <- State.inspect[AStarState, Option[Int]](_.gscore.get(cur).map(_ + 1)) 
              neighborGscore <- State.inspect[AStarState, Option[Int]](_.gscore.get(neighbor))
              _ <- 
               (stinkyGscore, neighborGscore) match {
                  case (Some(s), None) => 
                    updateNewScore(s, neighbor, cur, h)
                  case (Some(s), Some(n)) if (s < n) => 
                    updateNewScore(s, neighbor, cur, h)
                  case _ => State.pure[AStarState, Unit](())
               } 
            } yield ()
          }
        
        } yield None
      }
    } yield res.flatten
  }
  def astarImm(starts: Vector[Vec2i], goal: Vec2i, h: Vec2i => Int, graph: MountainMap): Option[List[Vec2i]] = {
    val cameFrom = Map[Vec2i, Vec2i]()
    val gscore = Map[Vec2i, Int].from(starts.map(_ -> 0))
    val fscore = Map[Vec2i, Int].from(starts.map(it => it -> h(it)))
    val queue = Heap.heapify(starts)(using Order.reverse(Order.by(it => fscore(it))))
    val state = AStarState(cameFrom, gscore, fscore, queue)
    astarMain(goal, h, graph).runA(state).value
  }
  def parse(str: String): (Vec2i, Vec2i, MountainMap) = {
    var start = Vec2i(0,0)
    var end = Vec2i(0, 0)
    var y = 0
    val values = str.lines().map { it =>
      var x = 0
      val res = it.map {
        case 'S' => {
          start = Vec2i(x, y)

          x += 1
          0
        }
        case 'E' => {
          end = Vec2i(x, y)
          x += 1
          25
        } 
        case c => 
          x += 1 
          c - 'a'
      }
      y += 1
      res.map(_.toByte)
    }.toScala(Vector).map(_.toVector)
    val grid = Grid(values)
    (start, end, MountainMap(grid))
  }

  val input = Source.fromResource("day12tst.txt").mkString 
  val (start, end, graph) = parse(input)
  // val DaState(_, _, prev) = dijstrasAlgorithim(graph, start)
  @annotation.tailrec
  final def extractPath(prev: Map[Vec2i, Vec2i], target: Vec2i, accum: List[Vec2i]): List[Vec2i] = {
    if (prev.isDefinedAt(target)) {
      val good =  prev(target)
      extractPath(prev, good, target :: accum)
    } else {
      (target :: accum).reverse
    }
  }

  def run2(input: String) = {
    val (start, end, graph) = parse(input)
    val starts = graph.filterIndex((i, b) => b == 0).map(_._1)
    val res = astar(starts, end, it => manhattanDistance(end, it), graph)
    println(res.get.size - 1)
  }
}
