package com.hbc.training

//private[training] only available in training
private[training] trait Lists {

  def cons[A](head:A, tail:ListR[A]):ListR[A] = Cons(head, tail)
  def nil[A]:ListR[A] = Nil
  def snoc[A](head:ListL[A], tail:A):ListL[A] = Snoc(head, tail)
  def lin[A]:ListL[A] = Lin

  def head[A]: ListR[A] => Maybe[A] = {
    case Cons(head, _) => just(head)
    case Nil => empty
  }

  def tail[A]: ListR[A] => Maybe[ListR[A]] = {
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
