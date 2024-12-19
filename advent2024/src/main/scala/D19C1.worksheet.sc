// https://adventofcode.com/2024/day/19
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D19.txt"
val example = "D19example.txt"

val input = Source.fromFile(filepath + filename).getLines()
val towels = input.next().split(", ")

var madePatterns: Set[String] = towels.toSet
var impossiblePatterns = Set[String]()

def makePattern(pattern: String): Boolean = {
  if (madePatterns contains pattern) return true
  else if (impossiblePatterns contains pattern) return false
  else
    val canMake = towels
      .filter(pattern.startsWith(_))
      .exists(t => makePattern(pattern.drop(t.length)))
    if (canMake) madePatterns += pattern
    else impossiblePatterns += pattern
    return canMake
}

var sum = 0
for (line <- input) {
  if (makePattern(line)) sum += 1
}
sum
