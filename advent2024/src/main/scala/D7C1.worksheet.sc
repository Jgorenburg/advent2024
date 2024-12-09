// https://adventofcode.com/2024/day/7
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D7.txt"
val example = "D7example.txt"

def canMakeResult(result: BigInt, inputs: List[BigInt]): Boolean = {
  def canMakeResultHelper(
      current: BigInt,
      remainingInputs: List[BigInt],
      debugPath: String
  ): Boolean = {
    if (remainingInputs.isEmpty) return {
      if (current == result) println(debugPath + " = " + result.toString)
      current == result
    }
    else if (current > result) return false
    else
      return canMakeResultHelper(
        current + remainingInputs.head,
        remainingInputs.tail,
        debugPath + "+" + remainingInputs.head.toString
      ) || canMakeResultHelper(
        current * remainingInputs.head,
        remainingInputs.tail,
        debugPath + "*" + remainingInputs.head.toString
      )
  }

  return canMakeResultHelper(inputs.head, inputs.tail, inputs.head.toString)
}

var sum: BigInt = 0
for (line <- Source.fromFile(filepath + filename).getLines) {
  val Array(result, cod) = line.split(": ")
  if (canMakeResult(BigInt(result), cod.split(" ").map(BigInt(_)).toList)) {
    sum += BigInt(result)
  }
}
sum
