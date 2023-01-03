import net.bulbyvr.common.*
import GridAxisSystem.* 
object Day22 {
  enum GridPos {
    case Solid, Open, GNil 
  }
  import GridPos.* 
  extension (s: Seq[GridPos]) {
    def trimPos: Seq[GridPos] = {
      s.dropWhile(_ == GNil).takeWhile(_ != GNil)
    }
  }
  case class ConnectionInfo(thisDir: Direction2D, thatDir: Direction2D, thatPos: Vec2i)
  case class Face(pos: Vec2i, connectsTo: Map[Direction2D, ConnectionInfo])
  case class ForbiddenGrid(grid: Grid[GridPos]) {
    // There is no such net where the width of the net at any given point is unequal to the width of
    // the cube
    // Unless it's sideways in which case WHY
    /*
    def getFaces: Iterable[Face] = {
      
      val faceWidth = grid.rows.map(_.trimPos.size).min
      val height = grid.height / faceWidth 
      val width = grid.width / faceWidth 
      val faces = (for {
        x <- 0 until width
        y <- 0 until height 
      } yield (x, y) -> (grid(x * faceWidth, y * faceWidth) != GNil)).toMap 
      assert(faces.values.count(identity) == 6)
      // Observations of cube nets: 
      // Faces that are directly connected don't matter for our purposes 
      // Faces that are positioned like so: 
      // .X.
      // .#X
      // Connect their sides right to top. This applies similarly to all mirrors of this 
      // Faces that are positioned like so: 
      // .X.
      // .#.
      // .#.
      // .X.
      // Directly connect top to bottom. 
      // Faces positioned like:
      // #X
      // #
      // X
      // Connect bottom to right
      faces.filter(_._2).keys.map { (x, y) => 
        val dNorth = faces.getOrElse((x, y - 1), false)
        val dSouth = faces.getOrElse((x, y + 1), false)
        val dEast = faces.getOrElse((x + 1, y), false)
        val dWest = faces.getOrElse((x - 1, y), false)
        val north = {
          if (dNorth) 
            None
          else {
            if (faces.getOrElse((x - 1, y - 1), false)) {
              // Connects like 
              // D.
              // .T
              Some(ConnectionInfo(Direction2D.Up, Direction2D.Right, Vec2i(x - 1, y - 1)))
            } else if (faces.getOrElse((x + 1, y - 1), false)) {
              // Connects like 
              // .D
              // T.
              Some(ConnectionInfo(Direction2D.Up, Direction2D.Left, Vec2i(x + 1, y - 1)))
            } else if (faces.getOrElse((x - 1, y - 2), false)) {
              // connects like 
              // D
              // #
              // #T
              Some(ConnectionInfo(Direction2D.Up, Direction2D.Right, Vec2i(x - 1, y - 2)))
            } else if (faces.getOrElse((x + 1, y - 2), false)) {
              // connects like 
              // .D
              // .#
              // T#
              Some(ConnectionInfo(Direction2D.Up, Direction2D.Left, Vec2i(x + 1, y - 2)))
            } 
          }
        }
        val east = {
          Option.when(!dEast) {
          
          }
        }
      }
       
    } 
    */
    /*
    final def moveRealCube(pos: Vec2i, dir: Direction2D, n: Int): Vec2i = {
      // real grid: 
      // .##
      // .#.
      // ##.
      // #..
      moveHelper(pos, dir, n) { (line, lastPos) => 
        val faceX = Math.floorDiv(lastPos.x, 50)
        val faceY = Math.floorDiv(lastPos.y, 50)
        val xEdge = lastPos.x % 50 
        val yEdge = lastPos.y % 50
        (faceX, faceY, dir) match {
          case (1, 0, Direction2D.Up) => 
            val destX = yEdge
            val destY = 0

        }
      }
    }
    */
    // helper means no recursion is accessable
    // @annotation.tailrec 
    final def move(pos: Vec2i, dir: Direction2D, n: Int): Vec2i = {
      def getGood(rowOrCol: Seq[GridPos]): Option[GridPos] = {
        dir.genAxisDirection match {
          case Axis2D.Direction.Positive => rowOrCol.find(_ != GNil)
          case Axis2D.Direction.Negative => rowOrCol.findLast(_ != GNil)
        }
      }
      def getPos(rowOrCol: Seq[GridPos]): Int = {
        dir.genAxisDirection match {
          case Axis2D.Direction.Positive => rowOrCol.indexWhere(_ != GNil)
          case Axis2D.Direction.Negative => rowOrCol.lastIndexWhere(_ != GNil)
        }
      }
      moveHelper(pos, dir, n) { (line, lastPos) =>
        val countUntilEdge = line.indexWhere(it => grid.getOrElse(it, GNil) == GNil)
        val restCount = n - countUntilEdge
        lazy val solidEnd = line.findLast(it => grid.getOrElse(it, GNil) != GNil)
        
        dir.axis match {
            case Axis2D.X => 
              val row = grid.extractRow(pos.y)
              val goodX = getGood(row) 
              goodX match
                // End movement
                case Some(Solid) => solidEnd.get
                // Start movement from that side 
                case Some(Open) =>

                  val x = getPos(row)
                  assert(x != -1)
                  move(pos.copy(x = x), dir, restCount)
                // Can't be correct
                case _ => !!! 
              
            case Axis2D.Y => 
              val col = grid.extractColumn(pos.x)
              val goodY = getGood(col)
              goodY match {
                case Some(Solid) => solidEnd.get
                case Some(Open) => 
                  val y = getPos(col)
                  assert(y != -1)
                  move(pos.copy(y = y), dir, restCount)
                case _ => !!!
              }
          }
      }
    }
    final def moveHelper(pos: Vec2i, dir: Direction2D, n: Int)(handleOff: (Seq[Vec2i], Vec2i) => Vec2i): Vec2i = {
      require(grid.getOrElse(pos, GNil) == Open)
      if (n == 0) return pos
      val newPos = pos.genOffset(dir, n)
      val line = pos `straightLine` newPos

      if (line.forall(it => grid.getOrElse(it, GNil) != Solid)) {
        if (line.forall(it => grid.getOrElse(it, GNil) != GNil)) {
          // simply return new pos 
          newPos 
        } else {
          handleOff(line, line.findLast( it => grid.getOrElse(it, GNil) != GNil).get)
        }
      } else {
        assert(grid.getOrElse(line.head, GNil) != Solid)
        val solidIdx = line.indexWhere(it => grid.getOrElse(it, GNil) == Solid)
        val openIdx = solidIdx - 1
        val openPos = line(openIdx)
        assert(grid.get(openPos).isDefined && grid(openPos) == Open)
        openPos
      } 
    }
  } 
  enum Instruction {
    case Clockwise
    case Counterclockwise
    case Move(n: Int)
  }
  def parseInstructions(input: String): List[Instruction] = {
    @annotation.tailrec 
    def helper(i: String, accum: List[Instruction]): List[Instruction] = {
      i match {
        case "" => accum.reverse 
        case _ => 
          i.head match {
            case 'R' => helper(i.tail, accum.prepended(Instruction.Clockwise))
            case 'L' => helper(i.tail, accum.prepended(Instruction.Counterclockwise))
            case _ => 
              val n = i.takeWhile(_.isDigit)
              val r = i.dropWhile(_.isDigit)
              helper(r, accum.prepended(Instruction.Move(n.toInt)))
          }
      }
    }
    helper(input.strip, List()) 
  }
  def parseMap(input: String): ForbiddenGrid = {
    val lines = 
      input.linesIterator.map { line => 
        line.map { 
          case ' ' => GNil 
          case '.' => Open 
          case '#' => Solid 
        }.toList 
      }.toList
    val width = lines.maxBy(_.length).length 
    val good = lines.map(_.padTo(width, GNil))
    ForbiddenGrid(Grid(good))
  }
  def parse(input: String): (ForbiddenGrid, List[Instruction]) = {
    val List(l, r) = input.split("\n\n").toList: @unchecked 
    (parseMap(l), parseInstructions(r))
  }

  def run(input: String): Int = {
    val (map, instr) = parse(input)
    val x = map.grid.rows.head.indexOf(Open)
    var pos = Vec2i(x, 0)
    var facing = Direction2D.Right 
    instr.foreach { it => 
      it match
        case Instruction.Clockwise => facing = facing.clockwise
        case Instruction.Counterclockwise => facing = facing.counterClockwise 
        case Instruction.Move(n) => pos = map.move(pos, facing, n)
      
    }
    val facingN = 
      facing match
        case Direction2D.Up => 3
        case Direction2D.Down => 1
        case Direction2D.Left => 2
        case Direction2D.Right => 0
    val rowN = (pos.y + 1) * 1000
    val colN = (pos.x + 1) * 4
    rowN + colN + facingN
  } 
}
