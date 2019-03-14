package com.hbc.training

private[training] trait Coproducts {
  final def injectedL[A,B](a:A):Coproduct[A,B] = InjectedL(a)
  final def injectedR[A,B](b:B):Coproduct[A,B] = InjectedR(b)

  final def left[A,B]: Coproduct[A,B] => Maybe[A] = {
    case InjectedL(a) => just(a)
    case _ => empty
  }

  final def right[A,B]: Coproduct[A,B] => Maybe[B] = {
    case InjectedR(b) => just(b)
    case _ => empty
  }

  final def mapC[A,B,C]: (B => C) => Coproduct[A,B] => Coproduct[A,C] = f => {
    case InjectedL(a) => injectedL[A,C](a)
    case InjectedR(b) => injectedR[A,C](f(b))
  }

//  final def bimap[A,B,C,D]: (A => C) => (B => D) => Coproduct[A,B] => Coproduct[Coproduct[C,B], Coproduct[A,D]] = ???
}
