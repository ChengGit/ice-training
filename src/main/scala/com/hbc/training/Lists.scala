package com.hbc.training

import scala.annotation.tailrec

//private[training] only available in training
private[training] trait Lists {

  /*
    ListR Operations
   */

  final def cons[A](head:A, tail:ListR[A]):ListR[A] = Cons(head, tail)
  final def nil[A]:ListR[A] = Nil

  final def mapListR[A,B]: (A => B) => ListR[A] => ListR[B] =
    f => as => reverseR(Fold[ListR].fold[A,ListR[B]](bs => a => cons(f(a), bs))(nil[B])(as))

  final def reverseR[A]: ListR[A] => ListR[A] = Fold[ListR].fold[A,ListR[A]](as => a => cons(a, as))(nil[A])

  final def sumR: ListR[Int] => Int =
    Fold[ListR].fold[Int,Int](b => a => a + b)(0)

//  final def fold[A,B]: (B => A => B) => B => ListR[A] => B = acc => zero => foldLoop(_, zero, acc)
//
//  @tailrec
//  private def foldLoop[A,B](list: ListR[A], acc: B, f: B => A => B): B = list match {
//    case Nil => acc
//    case Cons(h,t) => foldLoop(t, f(acc)(h), f)
//  }

  final def headR[A]: ListR[A] => Maybe[A] = {
    case Cons(head, _) => just(head)
    case Nil => empty
  }

  final def tailR[A]: ListR[A] => Maybe[ListR[A]] = {
    case Cons(_, tail) => just(tail)
    case Nil => empty
  }

  // need to comment this out and make the test pass with the implicit show function
  implicit final def showList[A](implicit S: Show[A]): Show[ListR[A]] = new Show[ListR[A]] {
    override def show: ListR[A] => String = {
      case Nil => "[]"
      case Cons(h, t) => s"[${h.show}${Fold[ListR].fold[A, String](s => a => s"$s,${a.show}")("")(t)}]"
    }
  }

  implicit final def foldList: Fold[ListR] = new Fold[ListR] {
    override def fold[A,B]: (B => A => B) => B => ListR[A] => B = acc => zero => foldLoop(_, zero, acc)

    @tailrec
    private def foldLoop[A,B](list: ListR[A], acc: B, f: B => A => B): B = list match {
      case Nil => acc
      case Cons(h,t) => foldLoop(t, f(acc)(h), f)
    }
  }

  /*
    ListL Operations
   */

  final def snoc[A](head:ListL[A], tail:A):ListL[A] = Snoc(head, tail)
  final def lin[A]:ListL[A] = Lin

  final def mapListL[A,B]: (A => B) => ListL[A] => ListL[B] = {
    func => listl => reverse(map(func, listl, lin[B]), lin)
  }

  @tailrec
  private def map[A,B](f:A => B, list: ListL[A], acc: ListL[B]): ListL[B] = list match {
    case Lin => acc
    case Snoc(h, t) => map(f, h, snoc(acc, f(t)))
  }

  final def reverseL[A]: ListL[A] => ListL[A] = {
    listl => reverse(listl, lin[A])
  }

  @tailrec
  private def reverse[A](list: ListL[A], acc: ListL[A]): ListL[A] = list match {
    case Lin => acc
    case Snoc(h, t) => reverse(h, snoc(acc, t))
  }

  final def sumL: ListL[Int] => Int = sumListL(_, 0)

  @tailrec
  private def sumListL[Int](listl: ListL[Int], acc: Int): Int = listl match {
    case Lin => acc
    case Snoc(h,t) => sumListL(h, acc + t)
  }

  final def headL[A]: ListL[A] => Maybe[ListL[A]] = {
    case Snoc(head, _) => just(head)
    case Lin => empty
  }

  final def tailL[A]: ListL[A] => Maybe[A] = {
    case Snoc(_, tail) => just(tail)
    case Lin => empty
  }

}
