package com.hbc.training

//private[training] only available in training
private[training] trait Lists {

  final def cons[A](head:A, tail:ListR[A]):ListR[A] = Cons(head, tail)
  final def nil[A]:ListR[A] = Nil
  final def snoc[A](head:ListL[A], tail:A):ListL[A] = Snoc(head, tail)
  final def lin[A]:ListL[A] = Lin

  final def sumR: ListR[Int] => Int =
    l => l.fold(0)(acc => i => i + acc)

  final def sumL: ListL[Int] => Int =
    l => l.fold(0)(acc => i => i + acc)

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
}
