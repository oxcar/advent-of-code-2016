
// Day 3: Squares With Three Sides
// https://adventofcode.com/2016/day/3

package com.oxcarh.adventofcode2016

import com.oxcarh.adventofcode2016.Tools._

import scala.util.matching.Regex

object Day3 extends App {

  val input = loadDayInputAsText(3).split("\n")

  implicit val linePattern = """[\s]*(\d+)[\s]*(\d+)[\s]*(\d+)[\s]*""".r

  // Solution 1 ------------------------------------------------------------

  val validTriangles = input.map(Triangle(_)).count(_.isValid)
  println(s"Solution 1: $validTriangles")

  // Solution 2 ------------------------------------------------------------

  val transposeInput = createTransposeInput(input)
  val validTriangles2 = transposeInput.foldLeft(0) { (validTriangles, row) =>
    row.grouped(3)
      .toList
      .map(t => Triangle(t(0), t(1), t(2)))
      .count(t => t.isValid) + validTriangles
  }
  println(s"Solution 2: $validTriangles2")

  // ----------------------------------------------------------------------

  case class Triangle(a: Int, b: Int, c: Int) {

    def isValid: Boolean = (a + b > c) && (a + c > b) && (b + c > a)
  }

  object Triangle {

    def apply(sides: String)(implicit pattern: Regex): Triangle = {
      sides match {
        case pattern(a, b, c) => new Triangle(a.toInt, b.toInt, c.toInt)
      }
    }
  }

  def createTransposeInput(lines: Array[String])(implicit pattern: Regex): Array[Array[Int]] = {
    lines.map {
      case pattern(a, b, c) => Array(a.toInt, b.toInt, c.toInt)
    }.transpose
  }

}
