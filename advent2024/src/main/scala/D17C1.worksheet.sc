// https://adventofcode.com/2024/day/17
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer

val filepath = "advent2024/src/main/resources/"
val filename = "D17.txt"
val example = "D17example.txt"

val computer = Source.fromFile(filepath + filename).getLines()
val RegisterA: Regex = raw"""Register A: (\d+)""".r
val RegisterB: Regex = raw"""Register B: (\d+)""".r
val RegisterC: Regex = raw"""Register C: (\d+)""".r
val Program: Regex = raw"""Program: (.*)""".r

var A = 0
var B = 0
var C = 0

var pointer = 0
var instructions = Array[Int]()
for (line <- computer) {
  line match
    case RegisterA(num) => A = num.toInt
    case RegisterB(num) => B = num.toInt
    case RegisterC(num) => C = num.toInt
    case Program(instructs) =>
      instructions = instructs.split(",").map(_.toInt)
    case _ =>
}

instructions
A

def combo(operand: Int): Int = {
  operand match
    case 4 => A
    case 5 => B
    case 6 => C
    case 7 => -1 // Should never occur
    case _ => operand
}

val outputs = ListBuffer[Int]()

while (pointer < instructions.length) {
  val operand = instructions(pointer + 1)
  instructions(pointer) match
    case 0 => A = A / (math.pow(2d, combo(operand).toDouble)).toInt
    case 1 => B = B ^ operand
    case 2 => B = combo(operand) % 8
    case 3 => if (A != 0) pointer = operand - 2
    case 4 => B = B ^ C
    case 5 => outputs += (combo(operand) % 8)
    case 6 => B = A / (math.pow(2d, combo(operand).toDouble)).toInt
    case 7 => C = A / (math.pow(2d, combo(operand).toDouble)).toInt
    case _ => println(f"recieved invalid instruction ${instructions(pointer)}")

  pointer += 2
}
outputs
outputs.mkString(",")
