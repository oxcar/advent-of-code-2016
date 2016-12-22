
// Day 22: Grid Computing
// https://adventofcode.com/2016/day/22

package com.oxcarh.adventofcode2016

import scala.collection.mutable.ArrayBuffer

object Day22 extends App {

  val input = Tools.loadDayInputAsText(day = 22).split("\n").drop(2)

  // Solution 1 --------------------------------------------------------------------------------------------------------

  val gridDimensions = calculateGridDimensions(input)
  val grid = ArrayBuffer.fill(gridDimensions._1)(ArrayBuffer.fill(gridDimensions._2)(Node(0, 0, 0, 0)))
  fillGrid(input, grid)

  val viablePairs = calculateViableNodePairs(grid)
  println(s"Solution 1: ${viablePairs.length}")

  // Solution 2 --------------------------------------------------------------------------------------------------------

  prettyPrintGrid(grid.transpose) // Solved by hand

  // -------------------------------------------------------------------------------------------------------------------

  def calculateGridDimensions(input: Array[String]): (Int, Int) = {
    val patt = """/dev/grid/node-x(\d*)-y(\d*)[\s]+(\d*)T[\s]+(\d*)T[\s]+(\d*)T[\s]+\d*%""".r
    val gridDimensions = input.foldLeft((0, 0)) { (gridSize, line) =>
      line match {
        case patt(posX, posY, _, _, _) =>
          val x = if (posX.toInt > gridSize._1) posX.toInt else gridSize._1
          val y = if (posY.toInt > gridSize._2) posY.toInt else gridSize._2
          (x, y)
        case _ => gridSize
      }
    }
    (gridDimensions._1 + 1, gridDimensions._2 + 1)
  }

  def fillGrid(input: Array[String], grid: ArrayBuffer[ArrayBuffer[Node]]): Unit = {
    val patt = """/dev/grid/node-x(\d+)-y(\d+)[\s]+(\d+)T[\s]+(\d+)T[\s]+(\d+)T[\s]+(\d+)%""".r
    input.foreach {
      case patt(posX, posY, size, used, avail, per) =>
        grid(posX.toInt)(posY.toInt) = Node(size.toInt, used.toInt, avail.toInt, per.toInt)
      case _ => ()
    }
  }

  def calculateViableNodePairs(grid: ArrayBuffer[ArrayBuffer[Node]]): Array[((Int, Int), (Int, Int))] = {
    val viablePairs = ArrayBuffer[((Int, Int), (Int, Int))]()
    for (i <- grid.indices; j <- grid(0).indices) {
      val node1 = grid(i)(j)
      for (i2 <- grid.indices; j2 <- grid(0).indices) {
        val node2 = grid(i2)(j2)
        if (!viablePairs.contains(((i, j), (i2, j2))) && node1.isViableNodePair(node2))
          viablePairs += (((i, j), (i2, j2)))
      }
    }
    viablePairs.toArray
  }

  // I need this to solve Part 2 by hand
  def prettyPrintGrid(grid: ArrayBuffer[ArrayBuffer[Node]]): Unit = {
    for (i <- grid.indices) {
      for (j <- grid(i).indices) {
        val node = grid(i)(j)
        if (i == 0 && j == 0) print("G ")
        else if (i == 0 && j == grid(i).length - 1) print("D ")
        else if (node.per > 90) print("# ")
        else if (node.used == 0) print("_ ")
        else print(". ")
      }
      println("")
    }
  }

  case class Node(size: Int, used: Int, avail: Int, per: Int) {

    def isViableNodePair(thatNode: Node): Boolean = {
      used > 0 && this != thatNode && thatNode.avail >= used
    }

  }

}
