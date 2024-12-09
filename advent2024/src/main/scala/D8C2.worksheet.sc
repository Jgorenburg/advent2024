// https://adventofcode.com/2024/day/8
import collection.mutable.{ListBuffer, Map}
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D8.txt"
val example = "D8example.txt"

val stage = Source.fromFile(filepath + filename).getLines.toArray
val vert = stage.length
val horiz = stage(0).length
val antennas = Map[Char, ListBuffer[(Int, Int)]]()

def makeAntinodes(a1: (Int, Int), a2: (Int, Int)): Set[(Int, Int)] = {
  val xDist = a1._1 - a2._1
  val yDist = a1._2 - a2._2
  var inLine = Set(a1, a2)
  var (x, y) = a1
  while (
    x >= 0 && x < horiz &&
    y >= 0 && y < vert
  ) {
    x += xDist
    y += yDist
    inLine += (x, y)
  }
  x = a2._1
  y = a2._2
  while (
    x >= 0 && x < horiz &&
    y >= 0 && y < vert
  ) {
    x -= xDist
    y -= yDist
    inLine += (x, y)
  }
  return inLine
}

for (
  i <- 0 until vert;
  j <- 0 until horiz
  if (stage(i)(j) != '.')
) {
  val c = stage(i)(j)
  if (!antennas.contains(c)) antennas(c) = ListBuffer()
  antennas(c) += ((j, i))
}

(for (key <- antennas.keys) yield {
//   antinodes = antinodes union
  antennas(key).toList
    .combinations(2)
    .flatMap {
      case a1 :: a2 :: _ => makeAntinodes(a1, a2)
      case _             => None
    }
    .filter(antinode =>
      antinode._1 >= 0 && antinode._1 < horiz &&
        antinode._2 >= 0 && antinode._2 < vert
    )
    .toSet
}).flatten.size
