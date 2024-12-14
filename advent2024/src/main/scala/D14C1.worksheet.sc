// https://adventofcode.com/2024/day/14
import scala.io.Source
import scala.util.matching.Regex

val filepath = "src/main/resources/"
val filename = "D14.txt"
val example = "D14example.txt"
val exampleDimensions = (11, 7)
val realDimensions = (101, 103)

val robots = Source.fromFile(filepath + filename).getLines()

val dimensions = realDimensions

object Quadrants {
  var topLeft = 0
  var topRight = 0
  var botLeft = 0
  var botRight = 0

  val xCutoff = dimensions._1 / 2
  val yCutoff = dimensions._2 / 2

  def addRobot(x: Int, y: Int) = {
    if (x > xCutoff && y > yCutoff) botRight += 1
    else if (x > xCutoff && y < yCutoff) topRight += 1
    else if (x < xCutoff && y > yCutoff) botLeft += 1
    else if (x < xCutoff && y < yCutoff) topLeft += 1
  }

  def getQuad(x: Int, y: Int): String = {
    if (x > xCutoff && y > yCutoff) return "botRight"
    else if (x > xCutoff && y < yCutoff) return "topRight"
    else if (x < xCutoff && y > yCutoff) return "botLeft"
    else if (x < xCutoff && y < yCutoff) return "topLeft"
    else return "None"
  }

  def getSafetyFactor(): Int = topLeft * topRight * botLeft * botRight

  def printQuads() = println(
    f"${topLeft}  ${topRight}  ${botLeft}  ${botRight}"
  )
}

val robotPosAndVel: Regex = raw"""p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".r
for (robot <- robots) {
  robot match
    case robotPosAndVel(px, py, vx, vy) =>
      var finalX =
        (dimensions._1 + (px.toInt + 100 * vx.toInt) % dimensions._1) % dimensions._1
      var finalY =
        (dimensions._2 + (py.toInt + 100 * vy.toInt) % dimensions._2) % dimensions._2
      Quadrants.addRobot(finalX, finalY)

    case _ => println(robot)
}
Quadrants.getSafetyFactor()
Quadrants.printQuads()
