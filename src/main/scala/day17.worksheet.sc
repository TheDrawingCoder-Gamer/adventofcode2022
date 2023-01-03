import scala.io.Source
import cats.*
import cats.implicits.*
import cats.data.*
import net.bulbyvr.common.*
import cats.collections.Dequeue 
import scala.collection.mutable as mut


val minusBlock = 
    Vector(
        Integer.parseUnsignedInt("0011110", 2).toByte
      )
val plusBlock = 
  Vector(
      Integer.parseUnsignedInt("0001000", 2).toByte, 
      Integer.parseUnsignedInt("0011100", 2).toByte,
      Integer.parseUnsignedInt("0001000", 2).toByte
    )
val lBlock = 
  Vector(
      Integer.parseUnsignedInt("0000100", 2).toByte,
      Integer.parseUnsignedInt("0000100", 2).toByte,
      Integer.parseUnsignedInt("0011100", 2).toByte
    ).reverse

val verticalBlock =
  Vector(
    Integer.parseUnsignedInt("0010000", 2).toByte,
    Integer.parseUnsignedInt("0010000", 2).toByte,
    Integer.parseUnsignedInt("0010000", 2).toByte,
    Integer.parseUnsignedInt("0010000", 2).toByte
    )

val blockBlock =
  Vector(
      Integer.parseUnsignedInt("0011000", 2).toByte,
      Integer.parseUnsignedInt("0011000", 2).toByte,
    )
val blocks = List(minusBlock, plusBlock, lBlock, verticalBlock, blockBlock)

enum BlockType {
  case Minus, Plus, LBlock, Vertical, BlockBlock
  def getBlock: Vector[Byte] = 
    this match
      case BlockType.Minus => minusBlock 
      case BlockType.Plus => plusBlock 
      case BlockType.LBlock => lBlock 
      case BlockType.Vertical => verticalBlock
      case BlockType.BlockBlock => blockBlock   
  def next = 
    BlockType.fromOrdinal((this.ordinal + 1) % BlockType.values.size)
}

case class BlockSeed(blockType: BlockType) {
  def block = blockType.getBlock 
  def next = BlockSeed(blockType.next)
}

def boolsToByte(bs: List[Boolean]) = boolsToByteImpl(bs, 0)
@annotation.tailrec 
final def boolsToByteImpl(bs: List[Boolean], accum: Byte): Byte = {
  bs match {
    case head :: next => 
      if (head) 
        boolsToByteImpl(next, ((accum << 1) | 1).toByte)
      else 
        boolsToByteImpl(next, (accum << 1).toByte)
    case Nil => 
      accum
  }
}

def collides(rows: Map[Int, Byte], block: Vector[Byte], height: Int) = {
  // get heights
  block
    .zip(Iterator.from(height))
    .exists((line, height) => rows.get(height).exists(it => (it & line) != 0))
}
def pushJet(block: Vector[Byte], dir: Char) = {
  val (pushable, res) = dir match {
    case '<' => block.forall(_ >> 6 == 0) -> block.map(it => (it << 1).toByte)
    case '>' => block.forall(rl => (rl & 1) == 0) -> block.map(it => (it >> 1).toByte)
    case _ => ???
  }
  if (pushable) res else block
}
def byteRow(pos: Int)(row: Iterable[Boolean]): Byte = 
  ((boolsToByte(row.toList) << row.size) >> pos) .toByte
case class TetrisGrid(rows: Map[Int, Byte], jets: LazyList[Int]) {
 }  
def showRow(b: Byte): String = {
  def toChar(bool: Boolean) = 
    if (bool) '#' else '.' 
  val r0 = (b & (1 << 6)) != 0
  val r1 = (b & (1 << 5)) != 0
  val r2 = (b & (1 << 4)) != 0
  val r3 = (b & (1 << 3)) != 0
  val r4 = (b & (1 << 2)) != 0
  val r5 = (b & (1 << 1)) != 0 
  val r6 = (b & (1 << 0)) != 0
  List(
    toChar(r0),
    toChar(r1),
    toChar(r2),
    toChar(r3),
    toChar(r4),
    toChar(r5),
    toChar(r6),
  ).mkString("","","")
}
/*
given tetrisShow: Show[TetrisGrid] = it => { 
  (for {
    n <- 0 to it.height 
  } yield showRow(it.rows.getOrElse(n, 0))).reverse.mkString("", "\n", "")
}
*/
case class DaState(seed: BlockSeed, grid: TetrisGrid)
val nextBlock: State[BlockSeed, Vector[Byte]] = State { state => 
  val nextOne = state.next
  (nextOne, state.block)
}
val nextBlockState: State[DaState, Vector[Byte]] = State { state => 
  val nextOne = state.seed.next 
  (state.copy(seed = nextOne), state.seed.block)
}


val input = Source.fromResource("day17tst.txt").mkString.strip()
val daJets =  Iterator.continually(input).flatten


/*
val place1Block: State[DaState, Unit] = 
  for {
    block <- nextBlockState 
    grid <- State.inspect[DaState, TetrisGrid](it => it.grid)
    newGrid = grid.placeBlock(block)
    _ <- State.modify[DaState](_.copy(grid = newGrid))
  } yield ()
*/

def addRockToChamber(chamber: Map[Int, Byte], rock: Vector[Byte], height: Int): Map[Int, Byte] = {
  rock
    .zip(Iterator.from(height))
    .foldLeft(chamber) { case (acc, (rockLine, daHeight)) =>
      acc + (daHeight -> (acc.getOrElse(daHeight, 0.toByte) | rockLine).toByte)
    }
}
def chamberHeight(chamber: Map[Int, Byte]) = if (chamber.isEmpty) 0 else chamber.keys.max + 1
def rockStream(n: Int): Iterator[Vector[Byte]] = Iterator.continually(blocks).flatten.take(n) 
def placeBlocks(initChamber: Map[Int, Byte], blocks: Iterator[Vector[Byte]], jets: Iterator[Char]): Map[Int, Byte] = {
  blocks
    .foldLeft(initChamber) { (chamber, nextRock) => 
    @annotation.tailrec 
    def pushAndDrop(rock: Vector[Byte], rockHeight: Int): (Vector[Byte], Int) = {
      val jetDir = jets.next()
      val pushed = pushJet(rock, jetDir)
      val afterPush = if (collides(initChamber, pushed, rockHeight)) rock else pushed 

      val canDrop = rockHeight > 0 && !collides(chamber, afterPush, rockHeight - 1)
      if (canDrop) pushAndDrop(afterPush, rockHeight - 1) else (afterPush, rockHeight)
    }
    val goodHeight = chamberHeight(chamber) + 3
    val (settledRock, settledHeight) = pushAndDrop(nextRock, goodHeight)
    addRockToChamber(chamber, settledRock, settledHeight) 
    
  }
}

def showChamber(chamber: Map[Int, Byte]): String = {
  if (chamber.isEmpty) "empty chamber"
  else {
    (0 until chamberHeight(chamber)).reverse.map { lineNo => 
      val r = Integer.toString(chamber.getOrElse(lineNo, 0.toByte), 2).replace('0', '.').replace('1', '#')
      val pr = "." * (7 - r.length) + r
      f"$lineNo%4d |$pr%s|"
    }.mkString("\n")

  }
}
showChamber(placeBlocks(Map.empty[Int, Byte], rockStream(3), daJets))


