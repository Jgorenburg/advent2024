// https://adventofcode.com/2024/day/11
import scala.io.Source
import scala.collection.mutable.{Map, Queue}

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

object Direction extends Enumeration {
  type Direction = Value
  val Up, Down, Left, Right = Value

  def moveDir(loc: (Int, Int), dir: Direction): (Int, Int) = {
    val offset = dir match
      case Up    => (1, 0)
      case Down  => (-1, 0)
      case Right => (0, 1)
      case Left  => (0, -1)
    return (loc._1 + offset._1, loc._2 + offset._2)
  }

  def adjacent(loc: (Int, Int), dir: Direction): List[(Int, Int)] = {
    val offsets = dir match
      case Up    => List((0, -1), (0, 1))
      case Down  => List((0, -1), (0, 1))
      case Right => List((1, 0), (-1, 0))
      case Left  => List((1, 0), (-1, 0))
    return offsets.map(offset => (loc._1 + offset._1, loc._2 + offset._2))
  }
}

import Direction.*

class region(letter: Char) {
  var area = 0
  var borderDirections: Map[Direction, Set[(Int, Int)]] =
    Direction.values.map(_ -> Set[(Int, Int)]()).to(Map)
  var perimeter = 0

  def getCost() = area * perimeter

  def addPlot(plot: (Int, Int)) = area += 1

  def checkSurrounding(plot: (Int, Int)): Queue[(Int, Int)] = {
    var newPlots = Queue[(Int, Int)]()
    for (dir <- Direction.values) {
      val pos = moveDir(plot, dir)
      if (inBounds(pos) && garden(pos._1)(pos._2) == letter) {
        if (!haveChecked(pos)) {
          newPlots += pos
        }
      } else {
        borderDirections(dir) += plot
        adjacent(plot, dir)
          .filter(loc => (borderDirections(dir).contains(loc)))
          .size match
          case 2 => perimeter -= 1
          case 0 => perimeter += 1
          case _ =>
      }
    }
    return newPlots
  }

  def handlePlot(plot: (Int, Int)): Queue[(Int, Int)] = {
    if (haveChecked(plot)) return Queue()
    checked(plot._1)(plot._2) = true
    addPlot(plot)
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
