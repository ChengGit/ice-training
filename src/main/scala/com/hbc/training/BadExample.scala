package com.hbc.training

private[training] trait BadExample {
  final def bad(limit: Int) = {
    //could be worse var could be outside
    var i = 0
    while (i <= limit) {
      //could be worse could be an insert in a table
      println(s"i=$i")
      i = i + 1
    }
  }
}

object BadExample extends BadExample {
  def main(args: Array[String]): Unit = {
    bad(17)
  }
}

