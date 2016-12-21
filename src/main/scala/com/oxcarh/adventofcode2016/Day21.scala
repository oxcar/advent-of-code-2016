
// Day 21: Scrambled Letters and Hash
// https://adventofcode.com/2016/day/21

package com.oxcarh.adventofcode2016

import scala.annotation.tailrec

object Day21 extends App {

  val input = Tools.loadDayInputAsText(day = 21).split("\n")

  // Solution 1 ------------------------------------------------------------

  var passwordInput = "abcdefgh"

  val patt1 = """rotate (right|left) (\d) step[s]*""".r
  val patt2 = """swap position (\d) with position (\d)""".r
  val patt3 = """rotate based on position of letter (\w)""".r
  val patt4 = """swap letter (\w) with letter (\w)""".r
  val patt5 = """reverse positions (\d) through (\d)""".r
  val patt6 = """move position (\d) to position (\d)""".r

  val scrambledPassword = input.foldLeft(passwordInput) { (passw, line) =>
    line match {
      case patt1(direction, steps) => rotate(passw, direction, steps.toInt)
      case patt2(pos1, pos2) => swapPosition(passw, pos1.toInt, pos2.toInt)
      case patt3(letter) => rotateOnLetterPosition(passw, letter)
      case patt4(letter1, letter2) => swapLetter(passw, letter1, letter2)
      case patt5(pos1, pos2) => reversePositions(passw, pos1.toInt, pos2.toInt)
      case patt6(pos1, pos2) => movePosition(passw, pos1.toInt, pos2.toInt)
      case _ => passw
    }
  }

  println(s"Solution 1: $scrambledPassword")

  // Solution 2 ------------------------------------------------------------

  val passwordInput2 = "fbgdceah"
  // Password permutations are used to brute force guess the reverse of Rotate on Letter Position
  implicit val passwordPermutations = "abcdefgh".toCharArray.permutations.map(_.mkString).toArray

  val unscrambledPassword = input.reverse.foldLeft(passwordInput2) { (passw, line) =>
    line match {
      case patt1(direction, steps) => reverseRotate(passw, direction, steps.toInt)
      case patt2(pos1, pos2) => swapPosition(passw, pos1.toInt, pos2.toInt)
      case patt3(letter) => reverseRrotateOnLetterPosition(passw, letter)
      case patt4(letter1, letter2) => swapLetter(passw, letter1, letter2)
      case patt5(pos1, pos2) => reversePositions(passw, pos1.toInt, pos2.toInt)
      case patt6(pos1, pos2) => movePosition(passw, pos2.toInt, pos1.toInt)
      case _ => passw
    }
  }

  println(s"Solution 2: $unscrambledPassword")

  // ----------------------------------------------------------------------

  @tailrec
  def rotate(s: String, direction: String, steps: Int): String = {
    if (steps == 0) s
    else {
      direction match {
        case "left" => rotate(s.tail + s.head, direction, steps - 1)
        case "right" => rotate(s.takeRight(1) + s.dropRight(1), direction, steps - 1)
        case _ => s
      }
    }
  }

  def reverseRotate(s: String, direction: String, steps: Int): String = {
    direction match {
      case "left" => rotate(s, "right", steps)
      case "right" => rotate(s, "left", steps)
      case _ => s
    }
  }

  def swapPosition(s: String, position1: Integer, position2: Integer): String = {
    val (p1, p2) = orderPositions(position1, position2)
    s.slice(0, p1) + s(p2) + s.slice(p1 + 1, p2) + s(p1) + s.slice(p2 + 1, s.length)
  }

  def rotateOnLetterPosition(s: String, letter: String): String = {
    val p = s.indexOf(letter)
    val steps = 1 + p + (if (p >= 4) 1 else 0)
    rotate(s, "right", steps)
  }

  def reverseRrotateOnLetterPosition(s: String, letter: String)(implicit passwordPermutations: Array[String]): String = {
    passwordPermutations
      .filter(rotateOnLetterPosition(_, letter) == s)
      .head
  }

  def swapLetter(s: String, l1: String, l2: String): String = swapPosition(s, s.indexOf(l1), s.indexOf(l2))

  def reversePositions(s: String, position1: Int, position2: Int): String = {
    val (p1, p2) = orderPositions(position1, position2)
    s.slice(0, p1) + s.slice(p1, p2 + 1).reverse + s.slice(p2 + 1, s.length)
  }

  def movePosition(s: String, position1: Int, position2: Int): String = {
    val letter = s(position1)
    val tmp = s.slice(0, position1) + s.slice(position1 + 1, s.length)
    tmp.slice(0, position2) + letter + tmp.slice(position2, s.length)
  }

  def orderPositions(p1: Int, p2: Int): (Int, Int) = {
    if (p1 < p2) (p1, p2) else (p2, p1)
  }

}
