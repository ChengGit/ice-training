package com.hbc.training

import scala.annotation.tailrec

//private[training] only available in training
private[training] trait Lists {

  def cons[A](head:A, tail:ListR[A]):ListR[A] = Cons(head, tail)
  def nil[A]:ListR[A] = Nil
  def snoc[A](head:ListL[A], tail:A):ListL[A] = Snoc(head, tail)
  def lin[A]:ListL[A] = Lin

  final def mapListR[A, B]: (A => B) => ListR[A] => ListR[B] =
    f => list => map(list, nil[B], f)

  @tailrec
  private def map[A,B](list:ListR[A], acc:ListR[B], f:A => B): ListR[B] = list match {
    case Nil => acc
    case Cons(h, t) => map(t, cons(f(h), acc), f)
  }

  def reverse[A]: ListR[A] => ListR[A] = ???

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
