import scala.util.matching.Regex
import collection.mutable.Set
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D6.txt"
val example = "D6example.txt"

val facility = Source.fromFile(filepath + filename).getLines.toArray
var visited = Set[(Int, Int)]()
val obstructions = Set[(Int, Int)]()
var loc = (-1, -1)

for (
  i <- facility.indices;
  j <- facility(i).indices
) {
  if (facility(i)(j) == '#') obstructions += ((j, i))
  else if (facility(i)(j) == '^') loc = (j, i)
}

object Direction extends Enumeration {
  type Direction = Value
  val Up, Right, Down, Left = Value

  def next(dir: Direction): Direction = {
    dir match
      case Up    => Right
      case Right => Down
      case Down  => Left
      case Left  => Up
  }
}

import Direction.*
val directions =
  Map(
    Up -> ((t: (Int, Int)) => (t._1 + 0, t._2 - 1)),
    Down -> ((t: (Int, Int)) => (t._1 + 0, t._2 + 1)),
    Right -> ((t: (Int, Int)) => (t._1 + 1, t._2)),
    Left -> ((t: (Int, Int)) => (t._1 - 1, t._2))
  )

var curDirection = Up

def inBounds(xBound: Int, yBound: Int)(pos: (Int, Int)): Boolean = {
  pos._1 >= 0 && pos._1 < xBound &&
  pos._2 >= 0 && pos._2 < yBound
}

val bounds = inBounds(facility(0).length, facility.length)

while (bounds(loc)) {
  visited += loc
  val newloc = directions(curDirection)(loc)
  if (obstructions contains newloc) curDirection = Direction.next(curDirection)
  else loc = newloc
}

visited.size
