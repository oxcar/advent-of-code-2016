package com.oxcarh.adventofcode2016

object Day19 extends App {

  println("Day 19: An Elephant Named Joseph")

  val numberOfElves = 3012210

  // Solution 1
  val elves = Array.fill[Int](numberOfElves)(1)
  while (elves.indexOf(numberOfElves) == -1) stealPresents(elves)
  println(s"Solution 1: ${elves.indexOf(numberOfElves) + 1}")

  // Solution 2
  var i = 1
  while (i * 3 < numberOfElves) i *= 3
  println(s"Solution 2: ${numberOfElves - i}")

  // ----------------------------------------------------------------------

  def stealPresents(elves: Array[Int]): Unit = {
    for (i <- elves.indices) {
      if (elves(i) != 0) {
        var j = if (i + 1 >= elves.length) 0 else i + 1
        while (elves(j) == 0) {
          j += 1
          if (j >= elves.length) j = 0
        }
        elves(i) = elves(i) + elves(j)
        elves(j) = 0
      }
    }
  }

}
