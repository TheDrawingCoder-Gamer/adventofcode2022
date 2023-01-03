package net.bulbyvr.common

case class Vec3i(x: Int, y: Int, z: Int) {
  def taxiDistance(that: Vec3i): Int = 
    Math.abs(this.x - that.x) + Math.abs(this.y - that.y) + Math.abs(this.z - that.z)
}
