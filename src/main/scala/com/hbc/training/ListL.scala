package com.hbc.training

sealed trait ListL[+A]

final case class Snoc[A](head:ListL[A], tail:A) extends ListL[A]


case object Lin extends ListL[Nothing] //How to construct a list = Snoc[Int](Snoc[Int](Nil,1),2)

