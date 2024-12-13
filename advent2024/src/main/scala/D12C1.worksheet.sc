// https://adventofcode.com/2024/day/12
import scala.io.Source
import scala.collection.mutable.Queue

val filepath = "src/main/resources/"
val myBand = "D12.txt"
val example = "D12example.txt"

val garden = Source
  .fromFile(filepath + myBand)
  .getLines()
  .toArray()
  .map(_.toArray)

val height = garden.length
val width = garden(0).length
def inBounds(loc: (Int, Int)): Boolean = {
  loc._1 >= 0 && loc._1 < height &&
  loc._2 >= 0 && loc._2 < width
}

var checked = Array.fill(height)(Array.fill(width)(false))
def haveChecked(plot: (Int, Int)): Boolean = checked(plot._1)(plot._2)

class region(letter: Char) {
  var area = 0
  var perimeter = 0

  def getCost() = area * perimeter

  def addPlot() = area += 1

  def checkSurrounding(plot: (Int, Int)): Queue[(Int, Int)] = {
    val dirToCheck = Queue((1, 0), (-1, 0), (0, 1), (0, -1))
      .map(offset => (plot._1 + offset._1, plot._2 + offset._2))
      .filter(pos => inBounds(pos) && garden(pos._1)(pos._2) == letter)

    perimeter += 4 - dirToCheck.length

    return dirToCheck.filter(!haveChecked(_))
  }

  def handlePlot(plot: (Int, Int)): Queue[(Int, Int)] = {
    if (haveChecked(plot)) return Queue()
    checked(plot._1)(plot._2) = true
    addPlot()
    return checkSurrounding(plot)
  }
}

var sum = 0
for (
  i <- 0 until height;
  j <- 0 until width;
  if (!haveChecked((i, j)))
) {
  val region = new region(garden(i)(j))
  var plots = Queue((i, j))
  while (plots.nonEmpty) {
    plots.enqueueAll(region.handlePlot(plots.dequeue()))
  }
  sum += region.getCost()
}
sum
