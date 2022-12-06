package com.tea

object Day6 extends App {

  val input = Input.getLines(6).head

  println(input)

  println(detect(input, 4))
  println(detect(input, 14))

  def detect(signal: String, len: Int) =
    signal
      .sliding(len)
      .zipWithIndex
      .dropWhile(_._1.distinct.length != len)
      .next()
      ._2 + len //+len to account for 0 index and sliding offset

}
