import net.bulbyvr.common.* 

object Day23 {
  type ElfGrid =  Grid[Boolean]

  case class Elf(current: Vec2i, proposal: Option[Vec2i])

  def propose(grid: ElfGrid, moveOrder: List[Direction2D], pos: Vec2i): Option[Elf] = {
    if (!grid(pos)) return None 

    val noe = grid.getOrElse(Vec2i(pos.x + 1, pos.y - 1), false)
    val n = grid.getOrElse(pos.copy(y = pos.y - 1), false)
    val nw = grid.getOrElse(Vec2i(pos.x - 1, pos.y - 1), false)
    val sw = grid.getOrElse(Vec2i(pos.x - 1, pos.y + 1), false)
    val se = grid.getOrElse(Vec2i(pos.x + 1, pos.y + 1), false)
    val s = grid.getOrElse(pos.copy( y = pos.y + 1), false)
    val e = grid.getOrElse(pos.copy(x = pos.x + 1), false)
    val w = grid.getOrElse(pos.copy(x = pos.x - 1), false)
    val defElf = Elf(pos, None)
    val northValid = !noe && !n && !nw 
    val southValid = !se && !s && !sw 
    val eastValid = !se && !e && !noe 
    val westValid = !sw && !w && !nw 
    moveOrder.find { it => 
      it match
        case Direction2D.Up => northValid 
        case Direction2D.Down => southValid 
        case Direction2D.Left => westValid 
        case Direction2D.Right => eastValid 
      
    }.map {
      case Direction2D.Down => defElf.copy(proposal = Some(pos.copy(y = pos.y + 1)))
      case Direction2D.Left => defElf.copy(proposal = Some(pos.copy(x = pos.x - 1)))
      case Direction2D.Right => defElf.copy(proposal = Some(pos.copy(x = pos.x + 1)))
      case Direction2D.Up => defElf.copy(proposal = Some(pos.copy(y = pos.y - 1)))
    }
  }
  val testInput = 
    """|....#..
       |..###.#
       |#...#.#
       |.#...##
       |#.###..
       |##.#.##
       |.#..#..""".stripMargin
  def round(grid: ElfGrid, moveOrder: List[Direction2D]): ElfGrid = {
    val elfs = 
      for {
        y <- 0 until grid.height 
        x <- 0 until grid.width if grid(x, y)
      } yield {
        propose(grid, moveOrder, Vec2i(x, y))
      }
    val goodElfs = elfs.flatten 
    val cuckedElfs = goodElfs.filter(_.proposal.isEmpty).map(_.current)
    val daProposals = goodElfs.filter(_.proposal.isDefined).groupBy(_.proposal.get)
    val goodPositions = daProposals.filter(_._2.size == 1).map(_._1)
    val otherPositions = daProposals.filter(_._2.size != 1).values.flatten.map(_.current)
    var daGrid = Grid.fill(grid.width + 2, grid.height + 2)(false)
    for {
      pos <- cuckedElfs ++ goodPositions ++ otherPositions  
    } do {
      daGrid = daGrid.updated(Vec2i(pos.x + 1, pos.y + 1))(true)
    }
    daGrid
  }
  def parse(input: String) = {
    Grid[Boolean](input.linesIterator.map(_.map {
      case '#' => true 
      case _ => false 
    }))
  }
  def run(input: String) = {
    val grid = parse(input)
    var resGrid = grid
    var order = List(Direction2D.Up, Direction2D.Down, Direction2D.Left, Direction2D.Right)
    for {
      _ <- 0 until 10 
    } {
      resGrid = round(resGrid, order)
      val head = order.head 
      order = order.tail.appended(head)
    }
    val rows = resGrid.rows 
    val cols = resGrid.columns 
    val topBound = rows.indexWhere(_.exists(identity))
    val botBound = rows.lastIndexWhere(_.exists(identity))
    val rightBound = cols.lastIndexWhere(_.exists(identity))
    val leftBound = cols.indexWhere(_.exists(identity))
    
    println(prettyShowBoolGrid(resGrid))
    resGrid.slice(Vec2i(leftBound, topBound), Vec2i(rightBound, botBound)).flatten.count(!_)
  }
  def prettyShowBoolGrid(grid: Grid[Boolean]): String = {
    grid.rows.map { it => 
      it.map(if (_) '#' else '.').mkString 
    }.foldLeft("")(_ + "\n" + _)
  }
}
