package com.tea

object Day4 extends App {

  val lines = Input.getLines(4)

  println(part1(lines))
  println(part2(lines))

  def ranges(line: String) =
    line.split(",")
      .map(_.split("-").toList match {
        case low :: high :: Nil => low.toInt to high.toInt
      }).toList


  def part1(lines: Seq[String]): Int =
    lines
      .map { line =>
        ranges(line) match {
          case range1 :: range2 :: Nil =>
            val intersect = range1 intersect range2
            Option.when((range1 forall intersect.contains) || (range2 forall intersect.contains))(1).getOrElse(0)
        }
      }.sum

  def part2(lines: Seq[String]): Int =
    lines
      .map { line =>
        ranges(line) match {
          case range1 :: range2 :: Nil => Option.when((range1 intersect range2).nonEmpty)(1).getOrElse(0)
        }
      }.sum
}
