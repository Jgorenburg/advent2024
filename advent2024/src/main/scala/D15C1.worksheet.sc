// https://adventofcode.com/2024/day/15
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

val filepath = "src/main/resources/"
val filename = "D15.txt"
val example1 = "D15example.txt"
val example2 = "D15example2.txt"

object Thing extends Enumeration {
  type Thing = Value
  val Robot, Box, Wall, Empty = Value
}

object Direction extends Enumeration {
  type Direction = Value
  val Up, Down, Left, Right = Value

  def moveDir(loc: (Int, Int), dir: Direction): (Int, Int) = {
    val offset = dir match
      case Up    => (-1, 0)
      case Down  => (1, 0)
      case Right => (0, 1)
      case Left  => (0, -1)
    return (loc._1 + offset._1, loc._2 + offset._2)
  }
}

import Thing.*
import Direction.*

val input = Source.fromFile(filepath + filename).getLines()
var stageBuffer = ArrayBuffer[Array[Thing]]()
var robotLoc: (Int, Int) = (0, 0)
var directionsBuffer = ListBuffer[Direction]()

var buildingStage = true
for (line <- input) {
  var row = ArrayBuffer[Thing]()
  if (buildingStage) {
    if (line.length < 2) buildingStage = false
    else {
      for (c <- line) {
        row += (c match {
          case '@' =>
            robotLoc = (stageBuffer.length, row.length)
            Robot
          case 'O' => Box
          case '#' => Wall
          case '.' => Empty
          case _ =>
            println(f"Invalid input ${c}")
            Empty
        })
      }
      stageBuffer += row.toArray
    }
  } else {
    for (c <- line) {
      directionsBuffer += (c match
        case '^' => Up
        case '>' => Right
        case 'v' => Down
        case '<' => Left
      )
    }
  }
}
var stage = stageBuffer.toArray
val directions = directionsBuffer.toList

robotLoc
for (direction <- directions) {
  val potentialMove = moveDir(robotLoc, direction)
  stage(potentialMove._1)(potentialMove._2) match
    case Empty =>
      stage(robotLoc._1)(robotLoc._2) = Empty
      stage(potentialMove._1)(potentialMove._2) = Robot
      robotLoc = potentialMove
    case Box =>
      var poi = potentialMove
      while (stage(poi._1)(poi._2) == Box) poi = moveDir(poi, direction)
      if (stage(poi._1)(poi._2) == Empty) {
        stage(robotLoc._1)(robotLoc._2) = Empty
        stage(poi._1)(poi._2) = Box
        stage(potentialMove._1)(potentialMove._2) = Robot
        robotLoc = potentialMove
      }

    case _ =>
}
robotLoc

var sum = 0
for (
  i <- stage.indices;
  j <- stage(i).indices
  if (stage(i)(j) == Box)
) {
  sum += i * 100 + j
}
sum
