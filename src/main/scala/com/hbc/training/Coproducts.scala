package com.hbc.training

private[training] trait Coproducts {
  def injectedL[A,B](a:A):Coproduct[A,B] = InjectedL(a)
  def injectedR[A,B](b:B):Coproduct[A,B] = InjectedR(b)
}
