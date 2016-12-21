package com.oxcarh.adventofcode2016

object Day21 extends App {

  val input = Tools.loadDayInputAsText(day = 21)

  var passwordInput = "abcdefgh"

  val p1 = """rotate (right|left) (\d) step[s]*""".r
  val p2 = """swap position (\d) with position (\d)""".r
  val p3 = """rotate based on position of letter (\w)""".r
  val p4 = """swap letter (\w) with letter (\w)""".r
  val p5 = """reverse positions (\d) through (\d)""".r
  val p6 = """move position (\d) to position (\d)""".r

  val scrambledPassword = input.split("\n").foldLeft(passwordInput) { (acc, line) =>
    line match {
      case p1(direction, steps) => rotate(acc, direction, steps.toInt)
      case p2(position1, position2) => swapPosition(acc, position1.toInt, position2.toInt)
      case p3(letter) => rotateOnLetterPosition(acc, letter)
      case p4(letter1, letter2) => swapLetter(acc, letter1, letter2)
      case p5(position1, position2) => reversePositions(acc, position1.toInt, position2.toInt)
      case p6(position1, position2) => movePosition(acc, position1.toInt, position2.toInt)
      case _ => acc
    }
  }

  println(s"Solution 1: $scrambledPassword")

  val passwordInput2 = "fbgdceah"
  implicit val passwordPermutations = "abcdefgh".toCharArray.permutations.map(_.mkString).toArray

  val unscrambledPassword = input.split("\n").reverse.foldLeft(passwordInput2) { (acc, line) =>
    line match {
      case p1(direction, steps) => reverseRotate(acc, direction, steps.toInt)
      case p2(position1, position2) => swapPosition(acc, position1.toInt, position2.toInt)
      case p3(letter) => reverseRrotateOnLetterPosition(acc, letter)
      case p4(letter1, letter2) => swapLetter(acc, letter1, letter2)
      case p5(position1, position2) => reversePositions(acc, position1.toInt, position2.toInt)
      case p6(position1, position2) => reverseMovePosition(acc, position1.toInt, position2.toInt)
      case _ => acc
    }
  }

  println(s"Solution 2: $unscrambledPassword")

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
    val slice1 = if (p1 > 0) s.slice(0, p1) else ""
    val slice2 = s(p1)
    val slice3 = if (p2 > p1 + 1) s.slice(p1 + 1, p2) else ""
    val slice4 = s(p2)
    val slice5 = if (p2 < s.length - 1) s.slice(p2 + 1, s.length) else ""
    slice1 + slice4 + slice3 + slice2 + slice5
  }

  def rotateOnLetterPosition(s: String, letter: String): String = {
    val p = s.indexOf(letter)
    val steps = 1 + p + {
      if (p >= 4) 1 else 0
    }
    rotate(s, "right", steps)
  }

  def reverseRrotateOnLetterPosition(s: String, letter: String)(implicit passwordPermutations: Array[String]): String = {
    passwordPermutations
      .filter(rotateOnLetterPosition(_, letter) == s)
      .head
  }

  def swapLetter(s: String, l1: String, l2: String): String = {
    val p1 = s.indexOf(l1)
    val p2 = s.indexOf(l2)
    swapPosition(s, p1, p2)
  }

  def reversePositions(s: String, position1: Int, position2: Int): String = {
    val (p1, p2) = orderPositions(position1, position2)
    val slice1 = if (p1 > 0) s.slice(0, p1) else ""
    val slice2 = s.slice(p1, p2 + 1).reverse
    val slice3 = if (p2 < s.length - 1) s.slice(p2 + 1, s.length) else ""
    slice1 + slice2 + slice3
  }

  def movePosition(s: String, position1: Int, position2: Int): String = {
    val letter = s(position1)
    val tmp = s.slice(0, position1) + s.slice(position1 + 1, s.length)
    tmp.slice(0, position2) + letter + tmp.slice(position2, s.length)
  }

  def reverseMovePosition(s: String, position1: Int, position2: Int): String = movePosition(s, position2, position1)

  def orderPositions(p1: Int, p2: Int): (Int, Int) = {
    if (p1 < p2) (p1, p2) else (p2, p1)
  }

}
