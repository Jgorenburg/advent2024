// https://adventofcode.com/2024/day/24
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map, Queue}
import scala.util.matching.Regex

val filepath = "advent2024/src/main/resources/"
val filename = "D24.txt"
val example = "D24example.txt"
val example2 = "D24example2.txt"
val input = Source.fromFile(filepath + filename).getLines()

val Wire: Regex = raw"""([a-zA-Z0-9]{3}): ([0-9])""".r
val Gate: Regex =
  raw"""([a-zA-Z0-9]{3}) ([A-Z]{2,3}) ([a-zA-Z0-9]{3}) -> ([a-zA-Z0-9]{3})""".r

var bitVals = Map[String, Int]()
var gateQueue = Queue[(String, String, String, String)]()
var zVals = Map[Int, Int]()

def tryGate(wire1: String, op: String, wire2: String, out: String): Unit = {
  if ((bitVals contains wire1) && (bitVals contains wire2)) {
    bitVals(out) = op match
      case "AND" => bitVals(wire1) & bitVals(wire2)
      case "OR"  => bitVals(wire1) | bitVals(wire2)
      case "XOR" => bitVals(wire1) ^ bitVals(wire2)
    if (out(0) == 'z') zVals(out.drop(1).toInt) = bitVals(out)
  } else { gateQueue.enqueue((wire1, op, wire2, out)) }
}

for (line <- input) {
  line match
    case Wire(name, bit) =>
      println(f"${name} ${bit}")
      bitVals(name) = bit.toInt
    case Gate(wire1, op, wire2, out) =>
      println(f"${wire1} ${op} ${wire2} ${out}")
      tryGate(wire1, op, wire2, out)

    case _ =>
}

while (gateQueue.nonEmpty) {
  val (wire1, op, wire2, out) = gateQueue.dequeue()
  tryGate(wire1, op, wire2, out)
}

bitVals
zVals
var sum = 0d
for (i <- 0 to zVals.keys.max) {
  sum += math.pow(2d, i) * zVals(i)
}
sum.toLong
