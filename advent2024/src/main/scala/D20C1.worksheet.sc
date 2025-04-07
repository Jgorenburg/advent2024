// https://adventofcode.com/2024/day/20
import scala.io.Source
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

val filepath = "advent2024/src/main/resources/"
val filename = "D20.txt"
val example = "D20example.txt"

val racecourse = Source.fromFile(filepath + filename).getLines().toArray

var start = (-1, -1)
breakable {
  for (
    i <- racecourse.indices;
    j <- racecourse(i).indices
    if (racecourse(i)(j) == 'S')
  ) {
    start = (i, j)
    break
  }
}
start

var prevLoc = (-1, -1)
var curLoc = start
var path = ListBuffer(start)

var temp = 0
val moves = List((-1, 0), (1, 0), (0, -1), (0, 1))

while (racecourse(curLoc._1)(curLoc._2) != 'E') {
  var nextLoc = moves
    .map(dir => (curLoc._1 + dir._1, curLoc._2 + dir._2))
    .filter(loc => loc != prevLoc && racecourse(loc._1)(loc._2) != '#')
    .head
  println(nextLoc)
  prevLoc = curLoc
  curLoc = nextLoc
  path += nextLoc
  temp += 1
}
path

var dist = path.zipWithIndex.toMap

var numCheats = 0
for (pos <- path) {
  val distToEnd = dist(pos)
  val validcheats = moves
    .map(dir => (pos._1 + dir._1, pos._2 + dir._2))
    .filter(loc => racecourse(loc._1)(loc._2) == '#')
    .map(loc => (2 * loc._1 - pos._1, 2 * loc._2 - pos._2))
    .filter(loc => (dist contains loc) && dist(loc) - distToEnd >= 102)

  println(f"${pos} ${validcheats}")
  numCheats += validcheats.length
}
numCheats
