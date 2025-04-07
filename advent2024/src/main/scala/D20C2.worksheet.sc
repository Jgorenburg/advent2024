// https://adventofcode.com/2024/day/20
import scala.io.Source
import scala.util.control.Breaks._
import scala.collection.mutable.ArrayBuffer

val filepath = "advent2024/src/main/resources/"
val filename = "D20.txt"
val example = "D20example.txt"

val racecourse = Source.fromFile(filepath + filename).getLines().toArray
// var save = 50 // for example
val save = 100 // for real

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
var pathBuffer = ArrayBuffer(start)

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
  pathBuffer += nextLoc
  temp += 1
}
val path = pathBuffer.toArray

def distance(p1: (Int, Int), p2: (Int, Int)): Int = {
  Math.abs(p1._1 - p2._1) + Math.abs(p1._2 - p2._2)
}

distance((0, 0), (5, 5))

var numCheats = 0
for (
  step <- path.indices;
  i <- step + save until path.length
) {
  val dist = distance(path(step), path(i))
  if (dist <= 20 && i - step - dist >= save) {
    numCheats += 1
    println(f"${path(step)}, ${path { i }}, ${dist}, ${i - step}")
  }
}
numCheats
