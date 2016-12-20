package com.oxcarh.adventofcode2016

// Day 20: Firewall Rules
// https://adventofcode.com/2016/day/20

object Day20 extends App {

  val input = Tools.loadDayInputAsText(day = 20)
  val maxIp = 4294967295L

  val rangos = input
    .split("\n")
    .map { line =>
      val rango = line.split("-").map(_.toLong)
      (rango(0), rango(1))
    }
    .sortWith(_._1 < _._1)

  val candidates = rangos.map(_._2 + 1).filter(_ <= maxIp)

  var allowedIps = Array[Long]()
  for (candiate <- candidates) {
    val banned = rangos.foldLeft(0) { (acc, rango) =>
      if (rango._1 <= candiate && candiate <= rango._2) acc + 1
      else acc
    }
    if (banned == 0) {
      allowedIps = allowedIps ++ Array(candiate)
    }
  }

  // ----------------------------------------------------------------------
  // Solution 1
  // ----------------------------------------------------------------------

  println(s"Solution 1: ${allowedIps(0)}")

  // ----------------------------------------------------------------------
  // Solution 2
  // ----------------------------------------------------------------------

  println(s"Solution 2: ${allowedIps.length}")
}
