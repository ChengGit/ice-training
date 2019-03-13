package com.hbc.training

private[training] trait Coproducts {
  def injectedL[A,B](a:A):Coproduct[A,B] = InjectedL(a)
  def injectedR[A,B](b:B):Coproduct[A,B] = InjectedR(b)

  def left[A,B]: Coproduct[A,B] => Maybe[A] = {
    case InjectedL(a) => just(a)
    case _ => empty
  }

  def right[A,B]: Coproduct[A,B] => Maybe[B] = {
    case InjectedR(b) => just(b)
    case _ => empty
  }



}
