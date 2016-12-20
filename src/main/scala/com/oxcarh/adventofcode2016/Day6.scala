
// Day 6: Signals and Noise
// https://adventofcode.com/2016/day/6

package com.oxcarh.adventofcode2016

object Day6 extends App {

  val input = Tools.loadDayInputAsText(day = 6)
  val transposedInput = input.split("\n").map(_.toCharArray).transpose

  // Solution 1 ------------------------------------------------------------

  val solution1 = transposedInput.foldLeft("") { (solution, line) =>
    solution + line
      .mkString
      .groupBy(_.toChar)
      .mapValues(_.length)
      .toList
      .sortWith(_._2 > _._2)
      .head
      ._1
  }
  println(s"Solution 1: $solution1")

  // Solution 2 ------------------------------------------------------------

  val solution2 = transposedInput.foldLeft("") { (solution, line) =>
    solution + line
      .mkString
      .groupBy(_.toChar)
      .mapValues(_.length)
      .toList
      .sortWith(_._2 < _._2)
      .head
      ._1
  }
  println(s"Solution 2: $solution2")

}
