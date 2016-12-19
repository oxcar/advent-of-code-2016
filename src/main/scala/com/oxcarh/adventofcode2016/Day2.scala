package com.oxcarh.adventofcode2016

import com.oxcarh.adventofcode2016.Tools._

object Day2 extends App {

  println("Day 2: Bathroom Security")

  val keypad1 = Array(
    Array("1", "2", "3"),
    Array("4", "5", "6"),
    Array("7", "8", "9")
  )

  val keypad2 = Array(
    Array("0", "0", "1", "0", "0"),
    Array("0", "2", "3", "4", "0"),
    Array("5", "6", "7", "8", "9"),
    Array("0", "A", "B", "C", "0"),
    Array("0", "0", "D", "0", "0")
  )

  val input = loadDayInputAsText(2).split("\n")

  // Soultion 1
  var state = State(Position(1, 1))
  implicit var keypad = keypad1
  var solution1 = Array[String]()
  input.foreach { line =>
    line.foreach { direction =>
      state = state.move(direction.toString)
    }
    solution1 = solution1 ++ Array(keypad(state.position.x)(state.position.y))
  }
  println(s"Solution 1: ${solution1.mkString}")

  // Soultion 2
  state = State(Position(1, 1))
  keypad = keypad2
  var solution2 = Array[String]()
  input.foreach { line =>
    line.foreach { direction =>
      state = state.move(direction.toString)
    }
    solution2 = solution2 ++ Array(keypad(state.position.x)(state.position.y))
  }
  println(s"Solution 2: ${solution2.mkString}")

  case class Position(x: Int, y: Int) {

    def isOutOfBounds()(implicit keypad: Array[Array[String]]): Boolean = {
      try {
        if (keypad(x)(y) == "0") true
        else false
      } catch {
        case ex: IndexOutOfBoundsException => true
      }
    }
  }

  case class State(position: Position) {

    def move(direction: String): State = {
      direction match {
        case "U" => State(safeMove(position, (-1, 0)))
        case "D" => State(safeMove(position, (1, 0)))
        case "L" => State(safeMove(position, (0, -1)))
        case "R" => State(safeMove(position, (0, 1)))
        case _ => this
      }
    }

    private def safeMove(position: Position, movement: (Int, Int)): Position = {
      val tmp = Position(position.x + movement._1, position.y + movement._2)
      if (tmp.isOutOfBounds()) position
      else tmp
    }
  }

}
