package com.hbc.training

private[training] trait Reverses {
  implicit def reverseListR: Reverse[ListR] = new Reverse[ListR] {
    override def reverse[A]: ListR[A] => ListR[A] = _.fold(nil[A])(as => a => cons(a,as))
  }

  implicit def reverseListL: Reverse[ListL] = new Reverse[ListL] {
    override def reverse[A]: ListL[A] => ListL[A] = _.fold(lin[A])(bs => a => snoc(bs, a))
  }

  implicit def reverseOps[M[_],A](m: M[A]): ReverseOps[M,A] = new ReverseOps[M,A](m)
}

final class ReverseOps[M[_],A](val m: M[A]) extends AnyVal {
  def reverse(implicit R: Reverse[M]): M[A] = R.reverse(m)
}