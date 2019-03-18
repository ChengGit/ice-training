package com.hbc.training

trait Map[M[_]] {
  def map[A,B]: (A => B) => M[A] => M[B]
}

object Map {
  def apply[M[_]](implicit M: Map[M]): Map[M] = M
}