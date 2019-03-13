package com.hbc.training

import com.hbc.training
import org.scalatest.{MustMatchers, WordSpecLike}

class MaybesTest extends Maybes with WordSpecLike with MustMatchers {

  "fold" should {

    "return the zero value for an empty maybe" in {
      fold[String, Int](0)(_.length)(training.empty) mustBe 0
    }

    "apply the function for a non-empty maybe" in {
      fold[String, Int](0)(_.length)(just("some")) mustBe 4
    }
  }

  "isDefined" should {

    "return false for an empty maybe" in {
      isDefined(training.empty) mustBe false
    }

    "return true for a non-empty maybe" in {
      isDefined(just(1)) mustBe true
    }
  }

  "getOrElse" should {

    "return the else value for an empty maybe" in {
      getOrElse(0).apply(training.empty) mustBe 0
    }

    "return the value from a non-empty maybe" in {
      getOrElse(0)(just(2)) mustBe 2
    }

    "return the value from a non-empty maybe, even if the zero would throw" in {
      getOrElse(throw new RuntimeException("Boom!"))(just(2)) mustBe 2
    }
  }
}
