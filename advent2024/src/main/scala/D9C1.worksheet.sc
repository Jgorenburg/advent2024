// https://adventofcode.com/2024/day/9
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

val filepath = "advent2024/src/main/resources/"
val filename = "D9.txt"
val example = "D9example.txt"

val line = Source.fromFile(filepath + filename).getLines.next()
var filesBuffer = ArrayBuffer[Long]()
var fileNum: Long = 0
var fillFromFrontBuffer = ArrayBuffer[Boolean]()

for ((digit, i) <- line.map(_.asDigit).zipWithIndex) {
  fillFromFrontBuffer = fillFromFrontBuffer ++ List.fill(digit)(i % 2 == 0)
  if (i % 2 == 0) {
    for (_ <- 0 until digit) {
      filesBuffer += fileNum
    }
    fileNum += 1;
  }
}

val files = filesBuffer.toArray
val fillFromFront = fillFromFrontBuffer.toArray.take(filesBuffer.length)
var front = 0
var back = files.length - 1
var checksum: Long = 0

for (pos <- 0 until filesBuffer.length) {
  if (fillFromFront(pos)) {
    checksum += pos * files(front)
    front += 1
  } else {
    checksum += pos * files(back)
    back -= 1
  }
  println(checksum)
}

checksum
