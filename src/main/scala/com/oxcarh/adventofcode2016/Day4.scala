
// Day 4: Security Through Obscurity
// https://adventofcode.com/2016/day/4

package com.oxcarh.adventofcode2016

import com.oxcarh.adventofcode2016.Tools.loadDayInputAsText

object Day4 extends App {

  val input = loadDayInputAsText(day = 4).split("\n")

  // Solution 1 ------------------------------------------------------------
  val validRoomsSectorIdCount = input.foldLeft(0) { (validRooms, line) =>
    val room = Room(line)
    if (room.isValid) validRooms + room.sectorId
    else validRooms
  }
  println(s"Solution 1: $validRoomsSectorIdCount")

  // Solution 2 ------------------------------------------------------------
  val room = input.map(Room)
    .filter(_.isValid)
    .filter(room => room.decryptRoomName.contains("north") || room.decryptRoomName.contains("pole"))
    .head
  println(s"Solution2: ${room.sectorId}")

  // ----------------------------------------------------------------------

  case class Room(roomHash: String) {

    private val roomPattern = """([a-z]*)(\d+)\[([a-z]*)\]""".r
    private val groups = roomPattern.findAllIn(roomHash.replaceAll("-", ""))
    private val frequencies = groups.group(1).toCharArray
      .groupBy(_.toChar)
      .mapValues(_.length)
      .toList
      .sortWith { case ((l1, c1), (l2, c2)) =>
        if (c1 > c2) true
        else if (c1 == c2 && l1 < l2) true
        else false
      }
    val sectorId: Int = groups.group(2).toInt
    private val checksum: String = groups.group(3)
    private val roomNamePattern = """([a-z\s]*)\d+\[[a-z]*\]""".r
    private val roomName = roomNamePattern.findAllIn(roomHash.replaceAll("-", " ")).group(1)

    def isValid: Boolean = {
      frequencies.slice(0, 5).map(_._1).mkString == checksum
    }

    def decryptRoomName: String = {
      roomName.map { c =>
        if (c != ' ') {
          var counter = sectorId
          var c2 = c.toInt
          while (counter > 0) {
            c2 = c2 + 1
            if (c2 > 'z'.toInt) c2 = 'a'.toInt
            counter = counter - 1
          }
          c2.toChar
        } else c
      }
    }
  }

}
