package net.bulbyvr.common

case class AABB private (start: Vec3i, end: Vec3i) {
  def contains(pos: Vec3i): Boolean = 
    (pos.x >= start.x) 
    && (pos.x <= end.x)
    && (pos.y >= start.y)
    && (pos.y <= end.y)
    && (pos.z >= start.z)
    && (pos.z <= end.z)

  def points: LazyList[Vec3i] = 
    for {
      x <- LazyList.from(start.x to end.x)
      y <- LazyList.from(start.y to end.y)
      z <- LazyList.from(start.z to end.z)
    } yield Vec3i(x, y, z)

  infix def intersect(that: AABB) = {
    Option.when(
      this.start.x <= that.end.x 
      && this.end.x >= that.start.x
      && this.start.y <= that.end.y
      && this.end.y >= that.start.y 
      && this.start.z <= that.end.z
      && this.end.z >= that.start.z 
      )(AABB(
        Vec3i(
          start.x max that.start.x,
          start.y max that.start.y, 
          start.z max that.start.z 
        ),
        Vec3i(
          end.x min that.end.x,
          end.y min that.end.y, 
          end.z min that.end.z
        )
      ))
  }

}

object AABB {
  def from(start: Vec3i, end: Vec3i): AABB = {
    val x1 = start.x `min` end.x 
    val y1 = start.y `min` end.y
    val z1 = start.z `min` end.z
    val x2 = start.x `max` end.x 
    val y2 = start.y `max` end.y
    val z2 = start.z `max` end.z
    AABB(Vec3i(x1, y1, z1), Vec3i(x2, y2, z2))
  }
}
