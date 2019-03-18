package com.hbc.training

private[training] trait Maps {
  implicit def mapListR: Map[ListR] = new Map[ListR] {
    override def map[A, B]: (A => B) => ListR[A] => ListR[B] =
      f => as => as.fold(nil[B])(acc => a => cons(f(a), acc)).reverse
  }

  implicit def mapOps[M[_],A](m: M[A]): MapOps[M,A] = new MapOps[M,A](m)
}

final class MapOps[M[_],A](val m: M[A]) extends AnyVal {
  def map[B](f: A => B)(implicit M: Map[M]): M[B] = M.map(f)(m)
}