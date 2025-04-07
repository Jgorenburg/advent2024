// https://adventofcode.com/2024/day/17
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

val filepath = "advent2024/src/main/resources/"
val filename = "D17.txt"

val computer = Source.fromFile(filepath + filename).getLines()
val Program: Regex = raw"""Program: (.*)""".r

var instructions = Array[Int]()
for (line <- computer) {
  line match
    case Program(instructs) =>
      instructions = instructs.split(",").map(_.toInt)
    case _ =>
}

instructions

def findOutput(startA: Long): Long = {
  return ((((startA % 8).toInt ^ 5) ^ 6) ^ (startA / (math.pow(
    2,
    ((startA % 8) ^ 5).toDouble
  ))).toLong) % 8
}

def findNext(target: Int, curA: Long): Set[Long] = {
  var inc = curA * 8
  val end = (curA + 1) * 8
  return (inc to end).filter(findOutput(_) == target).toSet
}

def reverseEngineer(): Long = {
  var targets = instructions.reverse.to(Queue)
  var possibilities = Set[Long](0)
  while (targets.nonEmpty) {
    val target = targets.dequeue
    possibilities = possibilities.map(p => findNext(target, p)).flatten
    // println(possibilities)
  }
  possibilities.min
}

reverseEngineer()
