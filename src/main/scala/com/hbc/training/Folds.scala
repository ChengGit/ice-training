package com.hbc.training

import scala.annotation.tailrec

private[training] trait Folds {
  implicit final def foldListR: Fold[ListR] = new Fold[ListR] {
    override def fold[A, B]: B => (B => A => B) => ListR[A] => B =
      acc => f => list => foldLoop(acc, f, list)

    @tailrec
    private final def foldLoop[A,B](acc: B, f: B => A => B, list: ListR[A]): B = list match {
      case Nil => acc
      case Cons(h,t) => foldLoop(f(acc)(h), f, t)
    }
  }

  implicit def foldOps[M[_],A](m: M[A]): FoldOps[M,A] = new FoldOps[M,A](m)
}

final class FoldOps[M[_],A](val m:M[A]) extends AnyVal {
  def fold[B](acc: B)(f: B => A => B)(implicit F: Fold[M]): B = F.fold(acc)(f)(m)
}
