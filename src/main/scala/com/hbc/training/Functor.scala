package com.hbc.training

trait Functor[M[_]] {
  def map[A,B]: (A => B) => M[A] => M[B]
}

object Functor{ //summoner pattern
  def apply[M[_]](implicit F: Functor[M]): Functor[M] = F
}
