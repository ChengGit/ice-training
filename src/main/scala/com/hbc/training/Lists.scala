package com.hbc.training

import scala.annotation.tailrec

//private[training] only available in training
private[training] trait Lists {

  def cons[A](head:A, tail:ListR[A]):ListR[A] = Cons(head, tail)
  def nil[A]:ListR[A] = Nil
  def snoc[A](head:ListL[A], tail:A):ListL[A] = Snoc(head, tail)
  def lin[A]:ListL[A] = Lin

  final def mapListR[A, B]: (A => B) => ListR[A] => ListR[B] = f => list => reverseR[B](mapListR(list, nil[B], f))

  final def reverseR[A]: ListR[A] => ListR[A] = reverseR(_, nil)

//  final def sumR: ListR[Int] => Int = sumR(_, 0)

  final def fold[A,B]: (B => A => B) => B => ListR[A] => B = acc => zero => foldLoop(_, zero, acc)

  final def sumR: ListR[Int] => Int = fold[Int,Int](b => a => a + b)(0)

  @tailrec
  private def foldLoop[A,B](list: ListR[A], acc: B, f: B => A => B): B = list match {
    case Nil => acc
    case Cons(h,t) => foldLoop(t, f(acc)(h), f)
  }

  @tailrec
  private def mapListR[A,B](list:ListR[A], acc:ListR[B], f:A => B): ListR[B] = list match {
    case Nil => acc
    case Cons(h, t) => mapListR(t, cons(f(h), acc), f)
  }

  @tailrec
  private def reverseR[A](list: ListR[A], acc: ListR[A]): ListR[A] = list match {
    case Nil => acc
    case Cons(h,t) => reverseR(t, cons(h, acc))
  }

//  @tailrec
//  private def sumR(list: ListR[Int], acc: Int): Int = list match {
//    case Nil => acc
//    case Cons(h,t) => sumR(t, acc + h)
//  }

  def headR[A]: ListR[A] => Maybe[A] = {
    case Cons(head, _) => just(head)
    case Nil => empty
  }

  def tailR[A]: ListR[A] => Maybe[ListR[A]] = {
    case Cons(_, tail) => just(tail)
    case Nil => empty
  }

  def headL[A]: ListL[A] => Maybe[ListL[A]] = {
    case Snoc(head, _) => just(head)
    case Lin => empty
  }

  def tailL[A]: ListL[A] => Maybe[A] = {
    case Snoc(_, tail) => just(tail)
    case Lin => empty
  }
}
