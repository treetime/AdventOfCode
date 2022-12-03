package com.tea

import scala.io.Source
import scala.util.ChainingSyntax

object Day2 extends App with ChainingSyntax {

  val lines =
    Source.fromFile("src/main/resources/day2")
      .getLines
      .toVector

  print(s"p1 -> ${part1(lines)}")
  print(s"p2 -> ${part2(lines)}")

  def part1(lines: Vector[String]): Int = {
    //A, X ROCK      1
    //B, Y PAPER     2
    //C, Z SCISSORS  3
    val wins = Seq("A Y", "B Z", "C X")
    val draws = Seq("A X", "B Y", "C Z")
    val moves =
      Map(
        'X' -> 1,
        'Y' -> 2,
        'Z' -> 3
      )
    lines
      .map { line =>
        Option.when(wins.contains(line))(6).getOrElse(0) +
          Option.when(draws.contains(line))(3).getOrElse(0) +
          line.lastOption.map(moves).getOrElse(0)
      }.sum
  }


  def part2(lines: Vector[String]): Int = {
    //X lose
    //Y draw
    //Z win
    val pWin = (c: Char) => (c.toInt - 64) % 3 + 1
    val pDraw = (c: Char) => (c.toInt - 64)
    val pLose = (c: Char) => (c.toInt - 63) % 3 + 1

    lines.map { line =>
      line.last match {
        case 'X' => pLose(line.head)
        case 'Y' => pDraw(line.head) + 3
        case 'Z' => pWin(line.head) + 6
      }
    }.sum
  }

}
