package com.hbc.training

private[training] trait Shows {
  implicit def showInt: Show[Int] = new Show[Int] {
    override def show: Int => String = i => s"Int[$i]"
  }

  implicit def showOps[A](a: A) = new ShowOps(a)
}

final class ShowOps[A](val a: A) extends AnyVal {
  def show(implicit S: Show[A]): String = S.show(a)
}
