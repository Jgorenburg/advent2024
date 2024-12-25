// https://adventofcode.com/2024/day/24
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map, Queue}
import scala.util.matching.Regex
import scala.util.Random

val filepath = "src/main/resources/"
val filename = "D24.txt"
val example3 = "D24example3.txt"
val input = Source.fromFile(filepath + filename).getLines()

val Wire: Regex = raw"""([a-zA-Z0-9]{3}): ([0-9])""".r
val Gate: Regex =
  raw"""([a-zA-Z0-9]{3}) ([A-Z]{2,3}) ([a-zA-Z0-9]{3}) -> ([a-zA-Z0-9]{3})""".r

// map of wires and the gates that feed into them
var wires = Map[String, (String, String, String)]()
var gateQueue = Queue[(String, String, String, String)]()
var zVals = 0
var zTransform = Map[Int, String]()

for (line <- input) {
  line match
    case Gate(wire1, op, wire2, out) =>
      wires(out) = (op, wire1, wire2)
      if (out(0) == 'z') {
        zVals = math.max(zVals, out.drop(1).toInt)
        zTransform(out.drop(1).toInt) = out
      }
    case _ =>
}

wires
zVals

wires("z00")

def add(x: String, y: String): String = {
  var output = ""
  var knownVals = Map[String, Int]()
  def getInput(wire: String): Int = {
    if (wire(0) == 'x')
      if (x(wire.drop(1).toInt) == '1') return 1
      else return 0
    else if (wire(0) == 'y')
      if (y(wire.drop(1).toInt) == '1') return 1
      else return 0
    else if (!knownVals.contains(wire)) {
      val (op, w1, w2) = wires(wire)
      knownVals(wire) = op match
        case "AND" => getInput(w1) & getInput(w2)
        case "OR"  => getInput(w1) | getInput(w2)
        case "XOR" => getInput(w1) ^ getInput(w2)
    }
    return knownVals(wire)
  }

  for (i <- 0 to zVals) {
    output += getInput(zTransform(i))
  }

  output.reverse
}

zVals

def getPattern(wire: String, indents: Int): Unit = {
  if (indents > 4) return
  if (wire(0) == 'x' || wire(0) == 'y')
    println("-" * indents + wire)
  else {
    val (op, w1, w2) = wires(wire)
    println("-" * indents + op + "  " + wire)
    getPattern(w1, indents + 1)
    getPattern(w2, indents + 1)
  }
}

getPattern("z40", 0)

var investigate = 0L
for (_ <- 0 to 1000) {
  val x = Random.nextLong(math.pow(2, 45).toLong)
  val y = Random.nextLong(math.pow(2, 45).toLong)
  val out = add(
    x.toBinaryString.reverse.padTo(zVals, '0'),
    y.toBinaryString.reverse.padTo(zVals, '0')
  )
  println(
    f"x: ${x} y: ${y} out: ${out} real: ${(x + y).toBinaryString} diff: ${(BigInt(out, 2) ^ (x + y)).toLong.toBinaryString}"
  )
  investigate = investigate | (BigInt(out, 2) ^ (x + y)).toLong

}

investigate.toBinaryString

// Did a semi-manual investigation
// dqr,dtk,pfw,shh,vgs,z21,z33,z39
