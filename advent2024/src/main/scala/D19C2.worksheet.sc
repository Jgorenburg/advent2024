// https://adventofcode.com/2024/day/19
import scala.io.Source
import collection.mutable.Map

val filepath = "src/main/resources/"
val filename = "D19.txt"
val example = "D19example.txt"

val input = Source.fromFile(filepath + filename).getLines()
val towels = input.next().split(", ")
input.next()

var waysToMakePattern = Map[String, Long]("" -> 1L)

def makePattern(pattern: String): Long = {
  if (!waysToMakePattern.contains(pattern))
    val nextSteps = towels
      .filter(pattern.startsWith(_))
    if (nextSteps.isEmpty) waysToMakePattern(pattern) = 0L
    else
      waysToMakePattern(pattern) =
        nextSteps.map(t => makePattern(pattern.drop(t.length))).sum
  return waysToMakePattern(pattern)
}

var sum = 0L
for (line <- input) {
  sum += makePattern(line)
}
sum
