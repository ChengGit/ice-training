package com.hbc.training

//+A declares A is covariant, same as `? extends` in java
sealed trait ListR[+A]

final case class Cons[A](head:A, tail:ListR[A]) extends ListR[A]


case object Nil extends ListR[Nothing] //How to construct a list = Cons[Int](1,Nil)

