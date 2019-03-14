package com.hbc.training

private[training] trait Show[A] {
  def show: A => String
}
