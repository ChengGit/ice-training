package com.hbc.training

trait Fold[C[_]] { //C[_] higher kind example
  def fold[A,B]: (B => A => B) => B => C[A] => B
}
