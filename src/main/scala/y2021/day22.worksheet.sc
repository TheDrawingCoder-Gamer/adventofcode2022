import cats.*
import cats.implicits.*
import net.bulbyvr.common.{Vec3i, AABB}
import scala.io.Source

case class Step(command: Boolean, bounds: AABB)

val input = Source.fromResource("2021/day22.txt")

