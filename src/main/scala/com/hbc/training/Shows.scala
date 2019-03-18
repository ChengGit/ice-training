package com.hbc.training

private[training] trait Shows {
  implicit def showInt: Show[Int] = new Show[Int] {
    override def show: Int => String = i => s"Int[$i]"
  }

  implicit def showFold[A,M[_]](implicit S: Show[A], F: Fold[M]): Show[M[A]] = new Show[M[A]] {
    override def show: M[A] => String = ms => ms.fold("")(s => a => s"$s,${a.show}") drop 1 match {
      case "" => "[]"
      case other => s"[$other]"
    }
  }

  implicit def showOps[A](a: A): ShowOps[A] = new ShowOps(a)
}

final class ShowOps[A](val a: A) extends AnyVal {
  def show(implicit S: Show[A]): String = S.show(a)
}
