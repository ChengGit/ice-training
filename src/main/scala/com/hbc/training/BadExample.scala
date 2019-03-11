package com.hbc.training

private[training] trait BadExample {
  final def bad(limit: Int) = {
    getValues(limit)
  }

  def getValues(limit:Int): Seq[Int] = 0 to limit

  def letPrint(str: String): Unit = {
    println(str)
  }

  def format(str: String): String = s"i=$str"
}

object BadExample extends BadExample {
  def main(args: Array[String]): Unit = {
    bad(17)
  }
}

