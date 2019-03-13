package com.hbc.training

trait HigherOrderFunctions {
  def something[A, B, C, D]: (A, B, C) => D = ???

  def something_else[A, B, C, D]: A => (B => (C => D)) = ???

  type Price = BigDecimal
  final case class Rate(value: BigDecimal)
  final case class Profile()

  //  def discount: Price => Price = _ * .9

  //  def discount_2: Profile => Price => Price = {
  //    case Profile(rate) => price => rate * price
  //  }
  //  def discount_3: (Profile => Rate) => Profile => Price => Price

  def discount: Rate => Price => Price = ???

  def rate: Profile => Rate = ???

  def variantPrice: (Rate => Price => Price) => (Profile => Rate) => Profile => Price => Price = ???

  val yourVariantPrice: Profile => Price => Price = variantPrice(discount)(rate)
}
