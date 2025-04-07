// https://adventofcode.com/2024/day/23
import scala.io.Source
import collection.mutable.{Map, Set}

val filepath = "advent2024/src/main/resources/"
val filename = "D23.txt"
val example = "D23example.txt"
val input = Source.fromFile(filepath + filename).getLines()

var connections = Map[String, Set[String]]()

def sharedThirds(c1: String, c2: String): Int =
  (connections(c1) intersect connections(c2)).size

def filteredThirds(c1: String, c2: String): Int =
  (connections(c1) intersect connections(c2)).filter(_(0) == 't').size

var triangles = 0
for (line <- input) {
  val Array(c1: String, c2: String) = line.split("-", 2)
  connections.getOrElseUpdate(c1, Set()) += c2
  connections.getOrElseUpdate(c2, Set()) += c1
  if (c1(0) == 't' || c2(0) == 't') {
    triangles += sharedThirds(c1, c2)
  } else {
    triangles += filteredThirds(c1, c2)
  }
}
triangles
