package com.hbc.training

import org.scalatest.{MustMatchers, WordSpecLike}

class BadExampleSpec extends WordSpecLike with MustMatchers with BadExample {
  "getValues" should {
    "work for negative limit value" in {
      getValues(-10) must be (Seq.empty)
    }
    "work for positive limit value" in {
      getValues(10) must contain theSameElementsAs expectedSeq
    }
    "work for 0 limit value" in {
      getValues(0) must contain theSameElementsAs zeroSeq
    }
  }
  private def expectedSeq = Seq(0,1,2,3,4,5,6,7,8,9,10)
  private def zeroSeq = Seq(0)
}
