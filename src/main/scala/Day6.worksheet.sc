import scala.io.Source

val input = Source.fromResource("day6.txt").mkString 
// val input = "bvwbjplbgvbhsrlpgdmjqwftvncz"
val n = 14 
input.sliding(n).indexWhere({ it => 
  it.combinations(2).forall(i => i.charAt(0) != i.charAt(1))
}) + n
