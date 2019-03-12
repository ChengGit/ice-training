package com.hbc.training

import com.hbc.training
import org.scalatest.{MustMatchers, WordSpecLike}

final class ListsTest extends WordSpecLike with MustMatchers with Lists {
  "Head" should {
    "return first element when list is not empty" in {
      head(cons(1, cons(2,nil))) mustBe just(1)
    }

    "return nothing when list is empty" in {
      head(nil) mustBe training.empty
    }
  }
}
