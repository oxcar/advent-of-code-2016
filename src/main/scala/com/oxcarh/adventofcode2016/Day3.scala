package com.oxcarh.adventofcode2016

import com.oxcarh.adventofcode2016.Tools._

object Day3 extends App {

  println("Day 3: Squares With Three Sides")

  val input = loadDayInputAsText(3).split("\n")

  val validTriangles = input.toList.foldLeft(0) { (validTriangles: Int, line) =>
    if (Triangle(line).isValid) validTriangles + 1
    else validTriangles
  }

  println(s"Solution 1: $validTriangles")

  case class Triangle(a: Int, b: Int, c: Int) {

    def isValid: Boolean = {
      (a + b > c ) && (a + c > b) && (b + c > a)
    }
  }

  object Triangle {

    def apply(sides: String): Triangle = {
      val pattern = """(\d+)[\s]*(\d+)[\s]*(\d+)""".r
      val groups = pattern.findAllIn(sides)
      new Triangle(groups.group(1).toInt, groups.group(2).toInt, groups.group(3).toInt)
    }
  }

}
