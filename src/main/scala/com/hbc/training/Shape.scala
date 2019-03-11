package com.hbc.training

sealed trait Shape

final case class Rectangle(width: Int, length: Int) extends Shape
final case class Circle(radius: Int) extends Shape
