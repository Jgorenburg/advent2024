// https://adventofcode.com/2024/day/9
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map}

val filepath = "src/main/resources/"
val filename = "D9.txt"
val example = "D9example.txt"

val line = Source.fromFile(filepath + filename).getLines.next()

// fileNum -> (defaultPos,size)
var files = Map[Long, (Long, Int)]()
var fileNum: Long = 0

// each pos is filled with the max-sized it can take
var emptySpacesBuffer = ArrayBuffer[Long]()

var fillLocs: Map[Long, Int] =
  Map() ++ (for (i <- 1 to 9) yield i.toLong -> -1)
var largest = 0

for ((digit, i) <- line.map(_.asDigit).zipWithIndex) {
  if (i % 2 == 0) {
    files(fileNum) = (emptySpacesBuffer.length, digit)
    fileNum += 1
  } else if (digit > largest) {
    for (m <- largest + 1 to digit) fillLocs(m) = emptySpacesBuffer.length
    largest = digit
  }
  for (internalPos <- 0 until digit) {
    if (i % 2 == 0) emptySpacesBuffer += 0
    else emptySpacesBuffer += digit - internalPos
  }
}

var emptySpaces = emptySpacesBuffer.toArray
files
fillLocs

var sum: Long = 0
for (f <- fileNum - 1 to 0 by -1) {
//   println(f"${f} ${files(f)}")
  val (default, size) = files(f)
  if (fillLocs(size) != -1 && emptySpaces(fillLocs(size)) < size) {
    // println(f"${size} ${fillLocs(size)}")
    var loc = fillLocs(size)
    while (loc != -1 && emptySpaces(loc) < size) {
      loc += 1
      if (loc >= emptySpaces.length) {
        loc = -1
      }
    }
    // println(f"loc ${loc}")
    fillLocs(size) = loc
    (size + 1 to 9)
      .filter(fillLocs(_) != -1)
      .map(x => fillLocs(x) = math.max(loc, fillLocs(x)))
  }
  if (fillLocs(size) == -1 || fillLocs(size) > default) {
    for (pos <- default until default + size) sum += f * pos
  } else {
    // println(f)
    // println(emptySpaces.toList)

    for (
      pos <- fillLocs(size) until
        fillLocs(size) + size
    ) {
      sum += f * pos
      //   println(f"${emptySpaces(pos)} ${fillLocs} ${pos}")
      emptySpaces(pos) = 0
      //   println(emptySpaces.toList)
    }
  }
}
sum
