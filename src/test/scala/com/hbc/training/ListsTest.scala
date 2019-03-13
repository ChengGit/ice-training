package com.hbc.training

import com.hbc.training
import org.scalatest.{MustMatchers, WordSpecLike}

final class ListsTest extends WordSpecLike with MustMatchers with Lists {
  "Head" should {
    "return first element when ListR is not empty" in {
      headR(cons(1, cons(2,nil))) mustBe just(1)
    }

    "return nothing when ListR is empty" in {
      headR(nil) mustBe training.empty
    }
  }

  "Tail" should {
    "return all elements after head when ListR is not empty" in {
      tailR(cons(1, cons(2,nil))) mustBe just(cons(2, nil))
    }

    "return nothing when ListR is empty" in {
      tailR(nil) mustBe training.empty
    }
  }

  "Map" should {
    "return a new list successfully" in {
      mapListR[Int, Int](_ + 3)(cons(2, nil)) mustBe cons(5, nil)
    }
  }
}
