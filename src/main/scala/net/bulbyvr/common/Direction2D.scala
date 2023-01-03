package net.bulbyvr.common

enum Direction2D {
  case Up, Down, Left, Right

  def axis = 
    this match
      case Direction2D.Up => Axis2D.Y 
      case Direction2D.Down => Axis2D.Y 
      case Direction2D.Left => Axis2D.X 
      case Direction2D.Right => Axis2D.X
  // Grid based axis direction
  // Like scratch for you NERDS
  def axisDirection = 
    this match
      case Direction2D.Up => Axis2D.Direction.Negative
      case Direction2D.Down => Axis2D.Direction.Positive
      case Direction2D.Left => Axis2D.Direction.Negative
      case Direction2D.Right => Axis2D.Direction.Positive
  def clockwise = 
    this match
      case Direction2D.Up => Direction2D.Right 
      case Direction2D.Down => Direction2D.Left 
      case Direction2D.Left => Direction2D.Up 
      case Direction2D.Right => Direction2D.Down 
  def counterClockwise = 
    this match
      case Direction2D.Up => Direction2D.Left
      case Direction2D.Down => Direction2D.Right
      case Direction2D.Left => Direction2D.Down
      case Direction2D.Right => Direction2D.Up 
}
