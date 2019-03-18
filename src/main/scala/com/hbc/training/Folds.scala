package com.hbc.training

import scala.annotation.tailrec

private[training] trait Folds {
  implicit final def foldListR: Fold[ListR] = new Fold[ListR] {
    override def fold[A, B]: B => (B => A => B) => ListR[A] => B =
      acc => f => list => foldLoopR(acc, f, list)

    @tailrec
    private final def foldLoopR[A,B](acc: B, f: B => A => B, list: ListR[A]): B = list match {
      case Nil => acc
      case Cons(h,t) => foldLoopR(f(acc)(h), f, t)
    }
  }

  implicit def foldListL: Fold[ListL] = new Fold[ListL] {
    override def fold[A, B]: B => (B => A => B) => ListL[A] => B = acc => f => as => foldLoopL(acc, f, as)

    @tailrec
    private final def foldLoopL[A,B](acc: B, f: B => A => B, as: ListL[A]): B = as match {
      case Lin => acc
      case Snoc(h,t) => foldLoopL(f(acc)(t), f, h)
    }
  }

  implicit def foldOps[M[_],A](m: M[A]): FoldOps[M,A] = new FoldOps[M,A](m)
}

final class FoldOps[M[_],A](val m:M[A]) extends AnyVal {
  def fold[B](acc: B)(f: B => A => B)(implicit F: Fold[M]): B = F.fold(acc)(f)(m)
}
