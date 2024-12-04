import scala.util.matching.Regex
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D4.txt"
var wordSearch: Array[String] = Array()
for (line <- Source.fromFile(filepath + filename).getLines) {
  wordSearch :+= line
}

val height = wordSearch.length
val width = wordSearch(0).length

var xmas = 0
for
  i <- 1 until height - 1
  j <- 1 until width - 1
  if (wordSearch(i)(j) == 'A')

  d1 = (for (k <- -1 to 1) yield wordSearch(i + k)(j + k)).mkString
  if (d1 == "MAS" || d1 == "MAS".reverse)

  d2 = (for (k <- -1 to 1) yield wordSearch(i + k)(j - k)).mkString
  if (d2 == "MAS" || d2 == "MAS".reverse)
do {
  xmas += 1
}
xmas
