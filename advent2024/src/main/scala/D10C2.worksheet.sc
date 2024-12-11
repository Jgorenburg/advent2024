// https://adventofcode.com/2024/day/10
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D10.txt"
val example = "D10example.txt"

val trailmap =
  Source
    .fromFile(filepath + filename)
    .getLines
    .toArray
    .map(_.toArray.map(_.asDigit))
val height = trailmap.length
val width = trailmap(0).length
var trails = Array.fill(height)(Array.fill(width)(-1))
val directions = List((1, 0), (-1, 0), (0, 1), (0, -1))

def getTrailCount(loc: (Int, Int)): Int = {
  if (trailmap(loc._1)(loc._2) == 9) return 1
  else if (trails(loc._1)(loc._2) == -1) {
    trails(loc._1)(loc._2) = directions
      .map(d => (d._1 + loc._1, d._2 + loc._2))
      .filter(pos =>
        pos._1 >= 0 && pos._1 < height &&
          pos._2 >= 0 && pos._2 < width &&
          trailmap(pos._1)(pos._2) == 1 + trailmap(loc._1)(loc._2)
      )
      .map(getTrailCount(_))
      .sum
  }

  return trails(loc._1)(loc._2)
}

var sum = 0
for (
  i <- trailmap.indices;
  j <- trailmap(i).indices
  if (trailmap(i)(j) == 0)
) { sum += getTrailCount((i, j)) }
sum
