import scala.jdk.StreamConverters.*

object Day3 {
  def parse(input : String) : List[Rucksack] = {
    input.lines().map(Rucksack.parse).toScala(List)
  }
  def run(input : String) : Int = {
    val data = parse(input)
    data.map(_.sharedItem.priority).sum
  }
  def runP2(input : String) : Int = {
    val data = parse(input)
    val groups = data.grouped(3).map(it => Group(it.head, it(1), it(2)))
    groups.map(_.badge.priority).sum
  }
}

case class ItemType(underlying : Char) {
  def priority : Int = {
    if (underlying.isUpper) {
      underlying - 'A' + 27
    } else {
      underlying - 'a' + 1
    }
  }
}
case class Rucksack(leftCompartment : List[ItemType], rightCompartment : List[ItemType]) {
  def isValid: Boolean = {
    invalidItems.isEmpty
  }
  def invalidItems: List[ItemType] = {
    leftCompartment.intersect(rightCompartment)
  }
  def sharedItem : ItemType = {
    invalidItems.head
  }
  def items : Set[ItemType] = (leftCompartment ++ rightCompartment).toSet
}
object Rucksack {
  def parse(input : String) : Rucksack = {
    val (l, r) = input.splitAt(input.length / 2)
    Rucksack(l.toList.map(ItemType.apply), r.toList.map(ItemType.apply))
  }
}
case class Group(elf1 : Rucksack, elf2 : Rucksack, elf3 : Rucksack) {
  def badge : ItemType = {
    elf1.items.intersect(elf2.items.intersect(elf3.items)).head
  }
}
