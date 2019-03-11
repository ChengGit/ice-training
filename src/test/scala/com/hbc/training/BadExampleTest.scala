package com.hbc.training

import org.scalatest.{MustMatchers, WordSpecLike}

final class BadExampleTest extends WordSpecLike with MustMatchers with BadExample {
  "produceValues" should {
    "work for negative limit value" in {
      produceValuesTo(-10) must be (Seq.empty)
    }

    "work for positive limit value" in {
      produceValuesTo(10) must contain theSameElementsAs expectedValue
    }

    "work for 0 limit value" in {
      produceValuesTo(0) must contain theSameElementsAs Seq(0)
    }
  }

  "format" should {
    "work for some number" in {
      format(123) must be("i=123")
    }
  }
  private def expectedValue = Seq(0,1,2,3,4,5,6,7,8,9,10)

}
