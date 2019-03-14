package com.hbc.training

private[training] trait Maybes {
  def just[A](a:A):Maybe[A] = Just(a)
  def empty[A]:Maybe[A] = Empty

  // Lifting, Functor, Endo-functor
  def map[A,B]: (A => B) => Maybe[A] => Maybe[B] =
    f => {
      case Just(a) => just(f(a))
      case _ => empty[B]
    }

  //guard clause
  def filter[A]: (A => Boolean) => Maybe[A] => Maybe[A] = {
    p => {
      case Just(a) if p(a) => just(a)
      case _ => empty
    }
  }

  implicit final def showMaybe[A]: Show[Maybe[A]] = new Show[Maybe[A]] {
    override def show: Maybe[A] => String = {
      case Just(a) => s"this is just a $a"
      case Empty => "this is empty"
    }
  }
}
