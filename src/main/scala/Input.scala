package com.tea

import scala.io.Source

object Input {

  def getLines(day: Int): Vector[String] = {
    val source = Source.fromFile(s"src/main/resources/day$day")
    val lines = source.getLines.toVector
    source.close()
    lines
  }

}
