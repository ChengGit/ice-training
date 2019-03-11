package com.hbc.training

private[training] trait BadExample {
  final def good(limit: Int) = {
    //could be worse var could be outside
    produceValuesTo(limit) map format
  }

  def produceValuesTo(limit: Int) : Seq[Int] = 0 to limit

  def format(value: Int) : String = s"i=$value"

//  what are the smaller functions?
  //produce values
  //format values
  //print values

}

object BadExample extends BadExample {
  def main(args: Array[String]): Unit = {

    println(good(17))
  }
}

