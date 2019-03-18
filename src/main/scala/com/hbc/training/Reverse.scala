package com.hbc.training

trait Reverse[M[_]] {
  def reverse[A]: M[A] => M[A]
}

object Reverse {
  def apply[M[_]](implicit R: Reverse[M]): Reverse[M] = R
}
