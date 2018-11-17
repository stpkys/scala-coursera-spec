package patmat

import scala.collection.JavaConverters._
import scala.collection.mutable

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution {
  def dayToInt(s: String): Int = {
    val days = List("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    days.indexOf(s)
  }

  def timeToMinutes(s: String): Int = {
    val k = s.split(":")
    val h = Integer.parseInt(k(0), 10)
    val m = Integer.parseInt(k(1), 10)
    h * 60 + m
  }

  def solution(s: String): Int = {
    val rawSlots = s.split("\n")
    val sch = rawSlots.map(t => {
      val p = t.split(" ")
      val day = dayToInt(p(0))
      val time = p(1).split("-").map(t => timeToMinutes(t) + day * 24 * 60)
      (time(0), time(1))
    })

    val slots = List((0, 0)) ::: sch.toList ::: List((7 * 24 * 60, 7 * 24 * 60))

    def findMaxSegment(max: Int, last: (Int, Int), t: List[(Int, Int)]): Int = {
      t match {
        case Nil => max
        case head :: tail =>
          val m = Math.max(max, head._1 - last._2)
          findMaxSegment(m, head, tail)
      }
    }

    val sorted = slots.sortBy(_._1)
    findMaxSegment(0, sorted.head, sorted.tail)
  }
}