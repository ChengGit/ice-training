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
    "return a new list successfully in the correct order" in {
      mapListR[Int, Int](_ + 3)(cons(2, cons(3, nil))) mustBe cons(5, cons(6, nil))
    }

    "return nil when list is empty" in {
      mapListR[Int,Int](_ + 3)(nil[Int]) mustBe nil[Int]
    }
  }

  "Reverse" should {
    "flip the order of a non-empty list" in {
      reverseR[Int](cons(1,cons(2, cons(3, nil)))) mustBe cons(3, cons(2, cons(1, nil)))
    }

    "return nil when list is empty" in {
      reverseR[Int](nil[Int]) mustBe nil[Int]
    }
  }


}
