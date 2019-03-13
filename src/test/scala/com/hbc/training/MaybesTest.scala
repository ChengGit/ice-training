package com.hbc.training

import com.hbc.training
import org.scalatest.{MustMatchers, WordSpecLike}

class MaybesTest extends WordSpecLike with MustMatchers with Maybes {
  "Map" should {
    "lift just a value correctly" in {
      map(factorial)(just(3)) mustBe just(6)
    }

    "lift an empty value" in {
      map(factorial)(training.empty) mustBe training.empty
    }
  }

  "Filter" should {
    "match a true predicate" in {
      filter[Int](_ > 2)(just(3)) mustBe just(3)
    }

    "match a false predicate" in {
      filter[Int](_ < 5)(just(7)) mustBe training.empty
    }

    "match a false predicate on an empty value" in {
      filter[Int](_ > 2)(training.empty) mustBe training.empty
    }
  }
}
