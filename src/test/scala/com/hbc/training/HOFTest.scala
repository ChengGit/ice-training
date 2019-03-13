package com.hbc.training

import org.scalatest.{MustMatchers, WordSpecLike}

class HOFTest extends WordSpecLike with MustMatchers {
  "Factorial" should {
    "calculate correctly for a non-zero num" in {
      factorial(2) mustBe 2
    }

    "calculate correctly for 0" in {
      factorial(0) mustBe 1
    }
  }
}
