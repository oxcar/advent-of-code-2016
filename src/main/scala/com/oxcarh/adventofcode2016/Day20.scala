package com.oxcarh.adventofcode2016

import scala.collection.mutable.ArrayBuffer

// Day 20: Firewall Rules
// https://adventofcode.com/2016/day/20

object Day20 extends App {

  val input = Tools.loadDayInputAsText(day = 20)
  val maxIp = 4294967295L

  val ipRanges = input
    .split("\n")
    .map { line =>
      val ipRange = line.split("-").map(_.toLong)
      (ipRange(0), ipRange(1))
    }
    .sortWith(_._1 < _._1)

  val candidates = ipRanges.map(_._2 + 1).filter(_ <= maxIp)
  val allowedIps = ArrayBuffer[Long]()

  for (candiate <- candidates) {
    val ipRangeCollision = ipRanges.foldLeft(0) { (acc, rango) =>
      if (rango._1 <= candiate && candiate <= rango._2) acc + 1
      else acc
    }
    if (ipRangeCollision == 0) allowedIps += candiate
  }

  // Solution 1 ------------------------------------------------------------
  println(s"Solution 1: ${allowedIps(0)}")

  // Solution 2 ------------------------------------------------------------
  println(s"Solution 2: ${allowedIps.length}")
}
