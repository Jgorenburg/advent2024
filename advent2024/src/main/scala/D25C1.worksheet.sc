// https://adventofcode.com/2024/day/25
import scala.io.Source
import scala.collection.mutable.ListBuffer

val filepath = "advent2024/src/main/resources/"
val filename = "D25.txt"
val example = "D25example.txt"

val input = Source.fromFile(filepath + filename).getLines()

var keyBuffer = ListBuffer[Array[Int]]()
var lockBuffer = ListBuffer[Array[Int]]()
var state = "Reset"
var pillars = Array(0, 0, 0, 0, 0)
for (line <- input) {
  if (state == "Reset") {
    state = line match
      case "#####" => "Lock"
      case "....." => "Key"
      case _ =>
        println(f"Issue, line should indicate key or lock: ${line}")
        "Reset"
  }

  if (line == "") {
    if (state == "Lock") lockBuffer += pillars
    else if (state == "Key") keyBuffer += pillars
    pillars = Array(0, 0, 0, 0, 0)
    state = "Reset"
  } else {
    for (
      i <- line.indices
      if (line(i) == '#')
    ) {
      pillars(i) += 1
    }
  }
}

val keys = keyBuffer.toList
val locks = lockBuffer.toList

keys.foreach(k => println(k.toList))
locks.foreach(l => println(l.toList))

var combos = 0
keys.foreach(k =>
  combos += locks.filter(l => (k zip l).map(_ + _).max < 8).length
)
combos
