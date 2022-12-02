package com.tea

import scala.io.Source
import scala.util.chaining._

object Day1 extends App {

  topN(1) pipe println
  topN(3) pipe println

  def topN(n: Int): Int =
    Source.fromFile("src/main/resources/day1")
      .getLines()
      .toVector
      .foldLeft(Seq(Seq.empty[Int])) {
        case (acc, elem) if elem.isBlank => Seq.empty +: acc
        case (acc, elem) => acc.headOption.map(l => (l :+ elem.toInt) +: acc.tail).getOrElse(acc)
      }
      .map(_.sum)
      .sorted
      .takeRight(n)
      .sum


}
