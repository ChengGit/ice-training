package com.hbc.training

import scala.annotation.tailrec

//private[training] only available in training
private[training] trait Lists {

  final def cons[A](head:A, tail:ListR[A]):ListR[A] = Cons(head, tail)
  final def nil[A]:ListR[A] = Nil
  final def snoc[A](head:ListL[A], tail:A):ListL[A] = Snoc(head, tail)
  final def lin[A]:ListL[A] = Lin

  final def mapListR[A,B]: (A => B) => ListR[A] => ListR[B] =
    f => as => reverseR(fold[A,ListR[B]](bs => a => cons(f(a), bs))(nil[B])(as))

  final def reverseR[A]: ListR[A] => ListR[A] = fold[A,ListR[A]](as => a => cons(a, as))(nil[A])

  final def sumR: ListR[Int] => Int = fold[Int,Int](b => a => a + b)(0)

  final def fold[A,B]: (B => A => B) => B => ListR[A] => B = acc => zero => foldLoop(_, zero, acc)

  @tailrec
  private def foldLoop[A,B](list: ListR[A], acc: B, f: B => A => B): B = list match {
    case Nil => acc
    case Cons(h,t) => foldLoop(t, f(acc)(h), f)
  }

  final def headR[A]: ListR[A] => Maybe[A] = {
    case Cons(head, _) => just(head)
    case Nil => empty
  }

  final def tailR[A]: ListR[A] => Maybe[ListR[A]] = {
    case Cons(_, tail) => just(tail)
    case Nil => empty
  }

  final def headL[A]: ListL[A] => Maybe[ListL[A]] = {
    case Snoc(head, _) => just(head)
    case Lin => empty
  }

  final def tailL[A]: ListL[A] => Maybe[A] = {
    case Snoc(_, tail) => just(tail)
    case Lin => empty
  }

  implicit final def showList[A](implicit S: Show[A]): Show[ListR[A]] = new Show[ListR[A]] {
    override def show: ListR[A] => String = fold[A, String](s => a => s"${S.show(a)},$s")("")(_)
  }
}
