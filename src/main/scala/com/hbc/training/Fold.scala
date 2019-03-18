package com.hbc.training

trait Fold[M[_]] { //M[_] higher kind example
  def fold[A,B]: B => (B => A => B) => M[A] => B
}

object Fold {
  def apply[M[_]](implicit F: Fold[M]): Fold[M] = F
}

