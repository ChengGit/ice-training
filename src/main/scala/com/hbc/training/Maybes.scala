package com.hbc.training

private[training] trait Maybes {
  def just[A](a:A):Maybe[A] = Just(a)
  def empty[A]:Maybe[A] = Empty
}
