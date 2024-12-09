// https://adventofcode.com/2024/day/2
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D2.txt"
var safe = 0
for (line <- Source.fromFile(filepath + filename).getLines.toList) {
  val levels = line.split(" ").map(_.toInt)
  if (levels.length <= 1) safe += 1
  else {
    val comp =
      if levels(1) > levels(0) then (x: Int, y: Int) => y - x < 4 && y > x
      else (x: Int, y: Int) => x > y && x - y < 4
    if (levels.sliding(2).forall(v => comp(v(0), v(1)))) safe += 1
  }
}
safe
