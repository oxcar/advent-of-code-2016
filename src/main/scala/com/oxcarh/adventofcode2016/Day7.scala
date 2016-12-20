
// Day 6: Internet Protocol Version 7
// https://adventofcode.com/2016/day/7

package com.oxcarh.adventofcode2016

object Day7 extends App {

  val input = Tools.loadDayInputAsText(day = 7)

  // Solution 1 -----------------------------------------------------------

  val solution1 = input
    .split("\n")
    .filter(hasAbba)
    .count(!hasAbbaInsideBrackets(_))

  println(s"Solution 1: $solution1")

  // ----------------------------------------------------------------------

  def hasAbba(ip7: String): Boolean = {
    val p = """.*([a-z])((?:(?!\1).))(\2)\1.*""".r
    p.findAllMatchIn(ip7).nonEmpty
  }

  def hasAbbaInsideBrackets(ip7: String): Boolean = {
    val p1 = """\[([a-z]*)\]""".r
    val p2 = """(?<=\[)[a-z]*([a-z])((?:(?!\1).))(\2)\1[a-z]*(?=\])""".r
    val hits = p1.findAllIn(ip7).count(p2.findAllIn(_).nonEmpty)
    hits > 0
  }

}
