
// Day 1: No Time for a Taxicab
// https://adventofcode.com/2016/day/1

package com.oxcarh.adventofcode2016

object Day1 extends App {

  val input = Tools.loadDayInputAsText(day = 1).split(", ")

  val pattern = """([RL])(\d+)""".r

  val inputTurns = input.map {
    case pattern("R", steps) => (Right, steps.toInt)
    case pattern("L", steps) => (Left, steps.toInt)
  }.toList

  val initialState = State(Position(0, 0), Directions.NORTH)
  val allStates = calculateAllStates(inputTurns, initialState)

  // Solution 1 ------------------------------------------------------------

  println(s"Solution 1: ${allStates.reverse.head.position.distanceTo(Position(0, 0))}")

  // Solution 2 ------------------------------------------------------------

  val firstStateRepeated = findFirstRepeated(allStates)
  println(s"Solution 2: ${firstStateRepeated.position.distanceTo(Position(0, 0))}")

  // ----------------------------------------------------------------------

  def calculateAllStates(input: List[(Turn, Int)], initialState: State): Array[State] = {
    var states = Array[State](initialState)
    input.foreach(i => {
      states = states ++ states.reverse.head.move1By1(i._1, i._2)
    })
    states
  }

  def findFirstRepeated(states: Array[State]): State = {
    var visitedPositions = Array[Position]()
    val statesIterator = states.toIterator
    while (statesIterator.hasNext) {
      val currentState = statesIterator.next()
      if (!visitedPositions.contains(currentState.position)) visitedPositions = visitedPositions ++ Array(currentState.position)
      else return currentState
    }
    states.head
  }

  case class Position(x: Int, y: Int) {

    def distanceTo(another: Position): Int = math.abs(x - another.x) + math.abs(y - another.y)
  }

  case class Direction(dx: Int, dy: Int) {

    def turn(turn: Turn): Direction = {
      this match {
        case Directions.NORTH => if (turn == Right) Directions.EAST else Directions.WEST
        case Directions.EAST => if (turn == Right) Directions.SOUTH else Directions.NORTH
        case Directions.SOUTH => if (turn == Right) Directions.WEST else Directions.EAST
        case Directions.WEST => if (turn == Right) Directions.NORTH else Directions.SOUTH
        case _ => Directions.NONE
      }
    }
  }

  case class State(position: Position, direction: Direction) {

    def move(turn: Turn, steps: Int): State = {
      val newDirection = direction.turn(turn)
      val newPosition = Position(position.x + (newDirection.dx * steps), position.y + (newDirection.dy * steps))
      State(newPosition, newDirection)
    }

    def move1By1(turn: Turn, steps: Int): Array[State] = {
      var states = Array[State]()
      val newDirection = direction.turn(turn)
      1 to steps foreach { s =>
        val newPosition = Position(position.x + newDirection.dx * s, position.y + newDirection.dy * s)
        states = states ++ Array(State(newPosition, newDirection))
      }
      states
    }
  }

  sealed trait Turn

  case object Right extends Turn

  case object Left extends Turn

  case object NoTurn extends Turn

  object Directions extends Enumeration {
    val NORTH = Direction(0, 1)
    val EAST = Direction(1, 0)
    val SOUTH = Direction(0, -1)
    val WEST = Direction(-1, 0)
    val NONE = Direction(0, 0)
  }

}
