package com.hbc.training

import org.scalatest.{MustMatchers, WordSpecLike}

final class CoproductsTest extends WordSpecLike with MustMatchers {
  "Map" should {
    "apply function if coproduct is a right" in {
      mapC[Int,Int,String](_.toString)(injectedR[Int,Int](5)) mustBe injectedR[Int,String]("5")
    }

    "not apply function if coproduct is a left" in {
      mapC[Int,Int,String](_.toString)(injectedL[Int,Int](3)) mustBe injectedL[Int,String](3)
    }
  }

}
