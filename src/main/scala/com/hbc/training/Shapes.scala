package com.hbc.training

trait Shapes {
  def area: Shape => Double = {
      case Circle(radius) => math.Pi * math.pow(radius, 2)
      case Rectangle(width,length) => width * length
    }
}
