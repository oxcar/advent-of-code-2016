package com.oxcarh.adventofcode2016

import com.oxcarh.adventofcode2016.Tools._

object Day3 extends App {

  println("Day 3: Squares With Three Sides")

  val input = loadDayInputAsText(3).split("\n")

  // Solution 1
  val validTriangles = input.map(Triangle(_)).count(_.isValid)
  println(s"Solution 1: $validTriangles")

  // Soultion 2
  val transposeInput = createTransposeInput(input)
  val validTriangles2 = transposeInput.foldLeft(0) { (validTriangles, row) =>
    row.grouped(3)
      .toList
      .map(t => Triangle(t(0), t(1), t(2)))
      .count(t => t.isValid) + validTriangles
  }
  println(s"Solution 2: $validTriangles2")

  // ----------

  case class Triangle(a: Int, b: Int, c: Int) {

    def isValid: Boolean = (a + b > c) && (a + c > b) && (b + c > a)
  }

  object Triangle {

    def apply(sides: String): Triangle = {
      val pattern = """(\d+)[\s]*(\d+)[\s]*(\d+)""".r
      val groups = pattern.findAllIn(sides)
      new Triangle(groups.group(1).toInt, groups.group(2).toInt, groups.group(3).toInt)
    }
  }

  def createTransposeInput(lines: Array[String]): Array[Array[Int]] = {
    lines.map { line =>
      val pattern = """(\d+)[\s]*(\d+)[\s]*(\d+)""".r
      val groups = pattern.findAllIn(line)
      Array(groups.group(1).toInt, groups.group(2).toInt, groups.group(3).toInt)
    }.transpose
  }

}
