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

    "return all elements before last element when ListL is not empty" in {
      headL(snoc(snoc(snoc(lin, 1), 2), 3)) mustBe just(snoc(snoc(lin, 1), 2))
    }

    "return nothing when ListL is empty" in {
      headL(lin) mustBe training.empty
    }
  }

  "Tail" should {
    "return all elements after first element when ListR is not empty" in {
      tailR(cons(1, cons(2,nil))) mustBe just(cons(2, nil))
    }

    "return nothing when ListR is empty" in {
      tailR(nil) mustBe training.empty
    }

    "return last element when ListL is not empty" in {
      tailL(snoc(snoc(snoc(lin, 1), 2), 3)) mustBe just(3)
    }

    "return nothing when ListL is empty" in {
      tailL(lin) mustBe training.empty
    }
  }

  "Fold" should {
    "return object accumulated from a ListR" in {
      cons(1, cons(2, cons(3, cons(4, nil)))).fold[Int](1)(acc => i => acc * i) mustBe 24
    }

    "return initial accumulator when ListR is empty" in {
      nil[Int].fold(1)(acc => i => acc * i) mustBe 1
    }

    "return object accumulated from a ListL" in {
      snoc(snoc(snoc(snoc(lin, 1), 2), 3), 4).fold(1)(acc => i => acc * i) mustBe 24
    }

    "return initial accumulator when ListL is empty" in {
      lin[Int].fold(1)(acc => i => acc * i) mustBe 1
    }
  }

  "Map" should {
    "return a new ListR successfully in the correct order" in {
      cons(2, cons(3, nil)).map[Int](_ + 3) mustBe cons(5, cons(6, nil))
    }

    "return nil when ListR is empty" in {
      nil[Int].map[Int](_ + 3) mustBe nil[Int]
    }

    "return a new ListL successfully in the correct order" in {
      snoc(snoc(snoc(lin, 1), 2), 3).map[Int](_ + 3) mustBe snoc(snoc(snoc(lin, 4), 5), 6)
    }

    "return lin when ListL is empty" in {
      lin[Int].map[Int](_ + 3) mustBe lin[Int]
    }
  }

  "Reverse" should {
    "flip the order of a non-empty ListR" in {
      cons(1,cons(2, cons(3, nil))).reverse mustBe cons(3, cons(2, cons(1, nil)))
    }

    "return nil when ListR is empty" in {
      nil[Int].reverse mustBe nil[Int]
    }

    "flip the order of a non-empty ListL" in {
      snoc(snoc(snoc(lin, 1), 2), 3).reverse mustBe snoc(snoc(snoc(lin, 3), 2), 1)
    }

    "return lin when ListL is empty" in {
      lin[Int].reverse mustBe lin[Int]
    }
  }

  "Sum" should {
    "add up all integers from a ListR of integers" in {
      sumR(cons(1, cons(2, cons(3, nil)))) mustBe 6
    }

    "return 0 when the ListR is empty" in {
      sumR(nil) mustBe 0
    }

    "add up all integers from a ListL of integers" in {
      sumL(snoc(snoc(snoc(lin, 1), 2), 3)) mustBe 6
    }

    "return 0 when the ListL is empty" in {
      sumL(lin) mustBe 0
    }
  }

  "Show" should {
    "return a string representing the ListR" in {
      cons(1,cons(2, cons(3,nil))).show mustBe "[Int[1],Int[2],Int[3]]"
    }

    "return a string representing the ListL" in {
      snoc(snoc(snoc(lin, 1), 2), 3).show mustBe "[Int[1],Int[2],Int[3]]"
    }
  }


}
