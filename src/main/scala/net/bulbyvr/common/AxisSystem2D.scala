package net.bulbyvr.common

trait AxisSystem2D {
  val rightPositive: Boolean
  val downPositive: Boolean

  extension (d: Direction2D) {
    def genAxisDirection: Axis2D.Direction = 
      d match
        case Direction2D.Up => 
          if (downPositive)
            Axis2D.Direction.Negative 
          else 
            Axis2D.Direction.Positive 
        case Direction2D.Down => 
          if (downPositive) 
            Axis2D.Direction.Positive
          else 
            Axis2D.Direction.Negative
        case Direction2D.Left =>
          if (rightPositive) 
            Axis2D.Direction.Negative
          else 
            Axis2D.Direction.Positive
        case Direction2D.Right =>
          if (rightPositive)
            Axis2D.Direction.Positive
          else 
            Axis2D.Direction.Negative
      
  }

  extension (p: Vec2i) {
    def genOffset(dir: Direction2D, n: Int = 1): Vec2i = {
      dir.axis match {
        case Axis2D.X => 
          dir.genAxisDirection match {
            case Axis2D.Direction.Positive => 
              p.copy(x = p.x + n)
            case Axis2D.Direction.Negative => 
              p.copy(x = p.x - n)
          }
        case Axis2D.Y => 
          dir.genAxisDirection match {
            case Axis2D.Direction.Positive => 
              p.copy(y = p.y + n)
            case Axis2D.Direction.Negative => 
              p.copy(y = p.y - n)
          }
      }
    }
  }
}

object GridAxisSystem extends AxisSystem2D {
  override val downPositive: Boolean = true 
  override val rightPositive: Boolean = true 
}
object HillAxisSystem extends AxisSystem2D {
  override val downPositive: Boolean = false 
  override val rightPositive: Boolean = true
} 
