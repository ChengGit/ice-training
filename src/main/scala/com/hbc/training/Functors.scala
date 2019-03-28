package com.hbc.training

private[training] trait Functors {
  implicit def functorOps[M[_],A](m: M[A]): FunctorOps[M,A] = new FunctorOps[M,A](m)
}

final class FunctorOps[M[_],A](val m: M[A]) extends AnyVal {
  def map[B](f: A => B)(implicit F: Functor[M]) : M[B] = F.map(f)(m)
}
