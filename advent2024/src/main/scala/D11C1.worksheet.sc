// https://adventofcode.com/2024/day/11
import scala.io.Source

val filepath = "advent2024/src/main/resources/"
val filename = "D11.txt"
val example = "D11example.txt"

var rocks: List[Long] =
  Source
    .fromFile(filepath + filename)
    .getLines()
    .next()
    .split(" ")
    .map(_.toLong)
    .toList

val maxDepth = 25

def handleRock(engraved: Long): List[Long] = {
  if (engraved == 0) return List(1)

  val sengraved = engraved.toString
  if (sengraved.length() % 2 == 0) {
    return List(
      sengraved.take(sengraved.length / 2).toInt,
      sengraved.drop(sengraved.length / 2).toInt
    )
  }

  return List(engraved * 2024)
}

for (_ <- 0 until maxDepth) {
  rocks = rocks.flatMap(handleRock(_))
}

rocks.length
