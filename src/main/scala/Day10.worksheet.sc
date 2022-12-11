import scala.io.Source
import cats.syntax.all.*
import cats.implicits.*
import cats.data.State
import net.bulbyvr.common.Grid

// I just KNOW it will tell me there is more than one register for part two
case class CPU(tick: Int, register: Int)

val defRegister = 'X'
sealed trait Operation
case object Noop extends Operation
case class Addx(n: Int) extends Operation

def opTakesCycles(op: Operation): Int = 
  op match
    case Noop => 1
    case Addx(_) => 2
  
val input = Source.fromResource("day10.txt").mkString 

val data = 
  (for (line <- input.linesIterator) yield {
    line.trim() match {
      case "noop" => Noop 
      case l  => 
        if (l.startsWith("addx "))
          Addx(l.drop(5).toInt)
        else 
          throw IllegalArgumentException()
    }
  }).toList


type CPUState[A] = State[CPU, A]

def executeOp(op: Operation): State[CPU, CPU] = State { it => 
  val newRegister = 
    op match
      case Noop => it.register
      case Addx(n) =>  it.register + n
  val newCpu = CPU(it.tick + opTakesCycles(op), newRegister)
  (newCpu, newCpu)
}
val solved: State[CPU, List[CPU]] = data.traverse(executeOp _)
val cpus = solved.runA(CPU(-1, 1)).value.toVector

def getCycle(cpus: Vector[CPU])(c: Int): CPU = {
  
  lazy val firstIndex = cpus.indexWhere(_.tick > c)
  lazy val lastIndex = cpus.lastIndexWhere(_.tick < c)
  cpus.findLast(_.tick < c).orElse(cpus.get(firstIndex - 1)).getOrElse(CPU(0, 1))
}
def getAndNormalize(cpus: Vector[CPU])(c: Int): CPU = {
  val cpu = getCycle(cpus)(c)
  cpu.copy(tick = c)
}
def signalStrengthPart1(cycle: Int)(cpu: CPU): Int = 
  cycle * cpu.register
val strengths = 
  for {
    c <- (20 to 220 by 40)
  } yield signalStrengthPart1(c)(getCycle(cpus)(c))
strengths.sum

type CRT = Grid[Boolean]
getCycle(cpus)(0)
val normalizedCpus = {
  val lastCycle = cpus.last.tick 
  for {
    c <- 0 to lastCycle 
  } yield getAndNormalize(cpus)(c)
}.toVector
def processCPU(cpu: CPU): State[CRT, CRT] = State { crt =>
  val good = (cpu.register - 1 to cpu.register + 1).contains(cpu.tick % crt.width)
  val newCrt = crt.updated(cpu.tick)(good)
  (newCrt, newCrt)
}

val solved2: State[CRT, CRT] = normalizedCpus.traverse(processCPU _).map(_.last)
val crt = solved2.runA(Grid[Boolean](List.fill(6, 40)(false))).value
def prettyShowBoolGrid(grid: Grid[Boolean]): String = {
  grid.rows.map { it => 
    it.map(if (_) '#' else '.').mkString 
  }.foldLeft("")(_ + "\n" + _)
}
prettyShowBoolGrid(crt)
