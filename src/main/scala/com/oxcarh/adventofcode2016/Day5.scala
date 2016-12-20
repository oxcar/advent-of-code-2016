
// Day 5: How About a Nice Game of Chess?
// https://adventofcode.com/2016/day/5

package com.oxcarh.adventofcode2016

import java.security.MessageDigest

object Day5 extends App {

  val input = Tools.loadDayInputAsText(day = 5)

  // Solution 1 ------------------------------------------------------------
  {
    var solution = Array[Char]()
    var counter: Long = 0
    while (solution.length < 8) {
      val (md5, n) = nextMD5(counter, input)
      solution = solution ++ Array(md5.charAt(5))
      counter = n
      println(s"n: $counter, letter: ${md5.charAt(5)}")
    }
    println(solution.mkString)
  }

  // Solution 2 ------------------------------------------------------------
  {
    val solution = Array.fill[Char](8)('X')
    var counter: Long = 0
    while (solution.contains('X')) {
      val (md5, n) = nextMD5(counter, input)
      counter = n
      val idx = md5.charAt(5).toString
      if("01234567".contains(idx) && solution(idx.toInt) == 'X'){
        solution(idx.toInt) = md5.charAt(6)
        println(s"n: $counter, letter: ${md5.charAt(6)}")
      }
    }
    println(solution.mkString)
  }

  // ----------------------------------------------------------------------

  def nextMD5(n: Long, input: String): (String, Long) = {
    var md5Hash = ""
    var counter: Long = n
    while (!isValidMD5(md5Hash)) {
      counter += 1
      md5Hash = calculateMD5(input + counter.toString)
    }
    (md5Hash, counter)
  }

  def calculateMD5(input: String): String = {
    MessageDigest.getInstance("MD5")
      .digest(input.getBytes)
      .map("%02X".format(_))
      .mkString
  }

  def isValidMD5(input: String): Boolean = input.startsWith("00000")

}
