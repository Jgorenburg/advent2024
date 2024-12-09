// https://adventofcode.com/2024/day/2
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D2.txt"
var safe = 0
for (line <- Source.fromFile(filepath + filename).getLines.toList) {
  val levels = line.split(" ").map(_.toInt)
  if (levels.length <= 2) safe += 1
  else {
    var comp =
      if levels(1) > levels(0) then (x: Int, y: Int) => y - x < 4 && y > x
      else (x: Int, y: Int) => x > y && x - y < 4
    val failPos = (levels.sliding(2).indexWhere(v => !comp(v(0), v(1))))
    if (failPos == -1) safe += 1
    else if
      val newLevels = levels.patch(failPos, Nil, 1);
      val newComp =
        if newLevels(1) > newLevels(0) then
          (x: Int, y: Int) => y - x < 4 && y > x
        else (x: Int, y: Int) => x > y && x - y < 4
      newLevels.sliding(2).forall(v => newComp(v(0), v(1)))
    then safe += 1
    else if
      val newLevels = levels.patch(failPos + 1, Nil, 1);
      val newComp =
        if newLevels(1) > newLevels(0) then
          (x: Int, y: Int) => y - x < 4 && y > x
        else (x: Int, y: Int) => x > y && x - y < 4
      newLevels.sliding(2).forall(v => newComp(v(0), v(1)))
    then safe += 1
    else if
      val newLevels = levels.patch(failPos - 1, Nil, 1);
      val newComp =
        if newLevels(1) > newLevels(0) then
          (x: Int, y: Int) => y - x < 4 && y > x
        else (x: Int, y: Int) => x > y && x - y < 4
      newLevels.sliding(2).forall(v => newComp(v(0), v(1)))
    then safe += 1
  }

}
safe
