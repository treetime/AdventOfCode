package com.tea

import scala.io.Source

object Day3 extends App {

  val source = Source.fromFile("src/main/resources/day3")
  val lines = source.getLines.toList
  source.close


  println(part1(lines))
  println(part2(lines))

  def priority(c: Char) =
    c.toInt match {
      case x if x <= 90 => x - 38
      case x => x - 96
    }

  def part1(lines: List[String]) =
    lines.flatMap { line =>
      line.splitAt(line.length / 2) match {
        case (left, right) =>
          (left intersect right)
            .headOption
            .map(priority)
      }
    }.sum

  def part2(lines: List[String]) =
    lines.grouped(3).flatMap {
      case a :: b :: c :: Nil =>
        (a intersect b intersect c)
        .headOption
          .map(priority)
    }.sum

}
