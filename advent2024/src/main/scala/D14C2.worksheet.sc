// https://adventofcode.com/2024/day/14
import scala.io.Source
import scala.util.matching.Regex
import scala.util.Using
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File

val filepath = "advent2024/src/main/resources/"
val filename = "D14.txt"
val example = "D14example.txt"
val exampleDimensions = (11, 7)
val realDimensions = (101, 103)

val robots = Source.fromFile(filepath + filename).getLines().toList

val dimensions = realDimensions

class Quadrants {
  var nearMid = 0

  val xCutoff = (dimensions._1 * 3 / 8, dimensions._1 * 5 / 8)
  val yCutoff = (dimensions._2 * 3 / 8, dimensions._2 * 5 / 8)

  def addRobot(x: Int, y: Int) = {
    if (
      x >= xCutoff._1 && x <= xCutoff._2 &&
      y >= yCutoff._1 && y <= yCutoff._2
    ) nearMid += 1
  }

  def midHeavy(): Boolean = {
    nearMid >= 100
  }

}

dimensions._1 * dimensions._2 / 32f
val q = new Quadrants
q.addRobot(1, 1)
q.xCutoff
q.yCutoff

26 * 26

val robotPosAndVel: Regex = raw"""p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".r
var file = "logs/christmastree.txt"

Using(BufferedWriter(FileWriter(File(file), false))) { bufferedWriter =>
  for (sec <- 0 until 10000) {
    val quads = new Quadrants
    var robotLocs = Set[(Int, Int)]()

    for (robot <- robots) {
      robot match
        case robotPosAndVel(px, py, vx, vy) =>
          var finalX =
            (dimensions._1 + (px.toInt + sec * vx.toInt) % dimensions._1) % dimensions._1
          var finalY =
            (dimensions._2 + (py.toInt + sec * vy.toInt) % dimensions._2) % dimensions._2
          if (!robotLocs.contains((finalX, finalY))) {
            quads.addRobot(finalX, finalY)
            robotLocs += (finalX, finalY)
          }

        case _ => println(robot)
    }
    if (quads.nearMid > 100) println(f"${quads.nearMid}")

    if (quads.midHeavy()) {
      bufferedWriter.write(f"${sec} (${quads.nearMid})")
      bufferedWriter.newLine()

      for (y <- 0 until dimensions._2) {
        bufferedWriter.write((for (x <- 0 until dimensions._1) yield {
          if (robotLocs contains (x, y)) 'X'
          else '.'
        }).mkString)
        bufferedWriter.newLine()

      }
    }
  }
}
