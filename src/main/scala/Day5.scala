package com.tea

object Day5 extends App {

  val lines = Input.getLines(5)
  val (stackLines, processLines) = lines.span(!_.isBlank)
  val pat = """move (\d+) from (\d+) to (\d+)""".r

  val stacks =
    stackLines
      .map(_.grouped(4).toList)
      .transpose
      .map(_.reverse)
      .map(x => x.tail.toList.filterNot(_.isBlank))


  val part1 = doWork(stacks, processLines, _.reverse)
  val part2 = doWork(stacks, processLines, identity)

  printRes(part1)
  printRes(part2)

  def doWork(
              stack: Vector[List[String]],
              process: Vector[String],
              dir: List[String] => List[String]) =
    process.tail.foldLeft(stack) { case (acc, elem) =>
      pat
        .findAllIn(elem)
        .matchData
        .flatMap(_.subgroups.map(_.toInt))
        .toSeq match {
        case amount :: from :: to :: Nil =>
          acc
            .updated(to - 1, acc(to - 1) ++ dir(acc(from - 1).takeRight(amount)))
            .updated(from - 1, acc(from - 1).dropRight(amount))
      }
    }

  def printRes(res: Vector[List[String]]) =
    println(
      res.flatMap(_.takeRight(1)).mkString("")
        .replace("""[""", "")
        .replace("""]""", "")
        .replace(""" """, "")
    )
}
