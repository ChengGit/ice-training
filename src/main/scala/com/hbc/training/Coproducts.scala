package com.hbc.training

private[training] trait Coproducts {
  def injectedL[A,B](a:A):Coproduct[A,B] = InjectedL(a)
  def injectedR[A,B](b:B):Coproduct[A,B] = InjectedR(b)

  def isInjectedL[A, B]: Coproduct[A,B] => Boolean = {
    case InjectedL(_) => true
    case InjectedR(_) => false
  }

  def isInjectedR[A,B]: Coproduct[A, B] => Boolean = {
    case InjectedL(_) => false
    case InjectedR(_) => true
  }

  def left[A, B]: Coproduct[A, B] => Maybe[A] = {
    case InjectedL(a) => just(a)
    case _ => empty
  }

  def right[A, B]: Coproduct[A, B] => Maybe[B] = {
    case InjectedR(b) => just(b)
    case _ => empty
  }
}
