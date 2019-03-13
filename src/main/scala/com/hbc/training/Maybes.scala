package com.hbc.training

private[training] trait Maybes {
  def just[A](a:A):Maybe[A] = Just(a)
  def empty[A]:Maybe[A] = Empty

  def fold[A, B](z: => B)(f: A => B): Maybe[A] => B = {
    case Empty => z
    case Just(a) => f(a)
  }

  def isDefined[A]: Maybe[A] => Boolean = {
    case Just(_) => true
    case _ => false
  }

  def isEmpty[A]: Maybe[A] => Boolean = a => !isDefined(a)

  def getOrElse[A](e: => A): Maybe[A] => A = {
    case Just(a) => a
    case _ => e
  }

  def map[A, B](f: A => B): Maybe[A] => Maybe[B] = {
    case Just(a) => just(f(a))
    case _ => empty[B]
  }

  def flatMap[A, B](f: A => Maybe[B]): Maybe[A] => Maybe[B] = {
    case Just(a) => f(a)
    case _ => empty[B]
  }
}
