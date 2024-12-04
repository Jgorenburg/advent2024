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
for (
  i <- 0 until height;
  j <- 0 until width;
  if (wordSearch(i)(j) == 'X')
) {
  // Horiz
  if (wordSearch(i).slice(j, j + 4) == "XMAS") xmas += 1
  if (wordSearch(i).slice(j - 3, j + 1) == "XMAS".reverse) xmas += 1

  // Vert
  if (
    height - i >= 4 &&
    (for (k <- 0 to 3) yield wordSearch(i + k)(j)).mkString == "XMAS"
  ) xmas += 1
  if (
    i >= 3 &&
    (for (k <- 0 to 3) yield wordSearch(i - k)(j)).mkString == "XMAS"
  ) xmas += 1

  // Diag
  if (
    height - i >= 4 && width - j >= 4 &&
    (for (k <- 0 to 3) yield wordSearch(i + k)(j + k)).mkString == "XMAS"
  ) xmas += 1
  if (
    i >= 3 && width - j >= 4 &&
    (for (k <- 0 to 3) yield wordSearch(i - k)(j + k)).mkString == "XMAS"
  ) xmas += 1
  if (
    height - i >= 4 && j >= 3 &&
    (for (k <- 0 to 3) yield wordSearch(i + k)(j - k)).mkString == "XMAS"
  ) xmas += 1
  if (
    i >= 3 && j >= 3 &&
    (for (k <- 0 to 3) yield wordSearch(i - k)(j - k)).mkString == "XMAS"
  ) xmas += 1

}
xmas
