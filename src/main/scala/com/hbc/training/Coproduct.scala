package com.hbc.training

sealed trait Coproduct[A,B]

final case class InjectedL[A,B](a:A) extends Coproduct[A,B]
final case class InjectedR[A,B](b:B) extends Coproduct[A,B]