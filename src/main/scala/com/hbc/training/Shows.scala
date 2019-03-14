package com.hbc.training

private[training] trait Shows {
  implicit def showInt: Show[Int] = new Show[Int] {
    override def show: Int => String = i => s"Int[$i]"
  }
}
