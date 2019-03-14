package com.hbc.training

trait Show[A] {
  def show: A => String
}

object Show {
  def apply[A](implicit S: Show[A]): Show[A] = S
}