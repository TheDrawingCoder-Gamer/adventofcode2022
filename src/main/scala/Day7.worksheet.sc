import scala.io.Source
import scala.collection.mutable as mut
import util.chaining.*
val input = Source.fromResource("day7.txt").mkString 
// val input = Source.fromResource("day7tst.txt").mkString
enum Command {
  case  Chdir(to: String)
  case List(files: Seq[AFile])
}
enum AFile(name: String) {
  case File(name: String, size: Int) extends AFile(name)
  case Dir(name: String) extends AFile(name)
}
// Returns 
def safeTakeWhile[A](it: Iterator[A])(f: A => Boolean): (Iterator[A], Option[A]) = {
  val daSeq = mut.ListBuffer[A]()
  while (it.hasNext) {
    val i = it.next() 
    if (f(i)) {
      daSeq += i 
    } else {
      return (daSeq.iterator, Option(i))
    }
  }
  (daSeq.iterator, None)
}
// drop cd /
val commands : Seq[Command] = {
  val daSeq = mut.ListBuffer[Command]() 
  val lines = input.linesIterator.toList
  var i = 0
  while (i < lines.length) {
    val line = lines(i)
    i += 1
    if (line.startsWith("$ ")) {
      val command = line.drop(2).trim().split(" ").toList match {
        case List[String]("cd", value) => daSeq += Command.Chdir(value)
        case List[String]("ls") => {
          val files = lines.drop(i).takeWhile { it => 
            if (!it.startsWith("$ ")) {
              i += 1 
              true 
            } else false
          }
          daSeq += Command.List(files.map { it => 
            if (it.startsWith("dir")) {
              AFile.Dir(it.drop(4))

            } else {
              val List[String](size, name) = it.split(" ").toList : @unchecked
              AFile.File(name, size.toInt)
            }
          })
        }
        case _ => throw IllegalArgumentException()
      }
    }
  }
  daSeq.toSeq
}.drop(1)


sealed trait FSFile {
  def size: Int 
  val name: String
  def show: String
}

case class FSFFile(size: Int, name: String) extends FSFile {
  override def show: String = {
    s"- $name (file, size=$size)"
  }
}
class FSDir(var size: Int, val name: String, val children: mut.ListBuffer[FSFile]) extends FSFile {
  override def toString: String = s"dir $name"
  override def show: String = {
    children.map(_.show.prependedAll("> ")).fold(s"- $name (dir)")(_ + "\n" + _)
  }
}
object FSDir {
}
val root: FSDir = FSDir(0, "/", mut.ListBuffer())
var workingOn: FSDir = root 
// sanity loss
val parents = mut.ListBuffer[FSDir]() 
commands.foreach {
  case Command.Chdir("..") => 
    workingOn = parents.head 
    parents.dropInPlace(1)
  case Command.Chdir(to) => {
    workingOn.children.find(_.name == to).tap(_.foreach { case it: FSDir =>
      parents.prepend(workingOn)
      workingOn = it
    }).getOrElse({
      val newOne = FSDir(0, to, mut.ListBuffer())
      workingOn.children += newOne
      parents.prepend(workingOn) 
      workingOn = newOne
    })
  }
  case Command.List(files) => 
    workingOn.children ++= files.map { 
      case AFile.Dir(name) => FSDir(0, name, mut.ListBuffer())
      case AFile.File(name, size) => FSFFile(size, name)
    }
}


final def calcSizes(fsdir: FSDir): Unit = {
  fsdir.children.foreach {
    case FSFFile(size, name) => ()
    case d: FSDir => calcSizes(d)
  }
  fsdir.size = fsdir.children.map(_.size).sum

}
calcSizes(root)

final def sizesLessThanN(fsdir: FSDir, n: Int): Seq[FSDir] = {
  lazy val children =
    fsdir.children.flatMap { 
      case it: FSDir =>
        sizesLessThanN(it, n)
      case _: FSFile => Seq()
    }.toSeq
  if (fsdir.size < n) {
    children.appended(fsdir)
  } else children
}
val test = FSDir(0, "/", mut.ListBuffer(FSDir(0, "a", mut.ListBuffer(FSDir(0, "b", mut.ListBuffer())))))
// sizesLessThanN(root, 100000).map(_.size).sum
val total = 70000000
val required = 30000000 
val used = root.size  
val available = total - used 
val toFree = required - available
final def findSmallestGreaterThanN(fsdir: FSDir, n: Int): Option[FSDir] = {
  val smallestChild = 
    fsdir.children.filter{
      case _: FSDir => true 
      case _ => false
    }.map(it => findSmallestGreaterThanN(it.asInstanceOf[FSDir], n)).minByOption(_.map(_.size).getOrElse(Int.MaxValue)).flatten
  val goodfsdir = 
    if (fsdir.size < n) 
      None 
    else 
      Some(fsdir)
  (goodfsdir, smallestChild) match {
    case (Some(parent), Some(child)) => 
      if (parent.size > child.size) 
        Some(child)
      else 
        Some(parent) 
    case _ => 
      goodfsdir.orElse(smallestChild)
  }

}

findSmallestGreaterThanN(root, toFree).map(_.size)
