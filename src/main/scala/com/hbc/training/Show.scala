package com.hbc.training

trait Show[A] {
  def show: A => String
}
