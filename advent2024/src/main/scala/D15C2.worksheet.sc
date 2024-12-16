// https://adventofcode.com/2024/day/15
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.mutable.Queue
import scala.collection.mutable.SortedSet

val filepath = "src/main/resources/"
val filename = "D15.txt"
val example1 = "D15example.txt"
val example2 = "D15example2.txt"
val example3 = "D15example3.txt"

object Thing extends Enumeration {
  type Thing = Value
  val Robot, LeftBox, RightBox, Wall, Empty = Value

  def isBox(thing: Thing): Boolean = thing == LeftBox || thing == RightBox
  def shiftBox(thing: Thing): Thing = {
    thing match
      case LeftBox  => RightBox
      case RightBox => LeftBox
      case _        => thing
  }
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
        row ++= (c match {
          case '@' =>
            robotLoc = (stageBuffer.length, row.length)
            Array(Robot, Empty)
          case 'O' => Array(LeftBox, RightBox)
          case '#' => Array(Wall, Wall)
          case '.' => Array(Empty, Empty)
          case _ =>
            println(f"Invalid input ${c}")
            Array(Empty, Empty)
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

def printStage(): Unit = {
  for (i <- stage.indices) {
    var str = ""
    for (j <- stage(i).indices) {
      str += (getThing(i, j) match
        case Robot    => '@'
        case LeftBox  => '['
        case RightBox => ']'
        case Wall     => '#'
        case Empty    => '.'
      )
    }
    println(str)
  }
}

def getThing(loc: (Int, Int)): Thing = stage(loc._1)(loc._2)
def getThing(y: Int, x: Int): Thing = getThing((y, x))
def setThing(loc: (Int, Int), thing: Thing): Unit = stage(loc._1)(loc._2) =
  thing
def setThing(y: Int, x: Int, thing: Thing): Unit = setThing((y, x), thing)

def shiftUpOrDown(robotLoc: (Int, Int), dir: Direction): Boolean = {
  val sign = if (dir == Up) then 1 else -1

  implicit val ordering: Ordering[(Int, Int)] =
    Ordering.by(t => sign * 100 * t._1 + t._2)
  var movingBoxes = SortedSet[(Int, Int)]()
  var toCheck = Queue[(Int, Int)]()

  def addBox(boxLoc: (Int, Int)): Unit = {
    val otherPart =
      if (getThing(boxLoc) == LeftBox) then (boxLoc._1, boxLoc._2 + 1)
      else (boxLoc._1, boxLoc._2 - 1)
    movingBoxes ++= SortedSet(otherPart, boxLoc)
    toCheck.enqueueAll(Set(boxLoc, otherPart))
  }

  addBox(moveDir(robotLoc, dir))

  while (toCheck.nonEmpty) {
    val box = toCheck.dequeue()
    val moveTo = moveDir(box, dir)
    getThing(moveTo) match
      case Empty    =>
      case LeftBox  => addBox(moveTo)
      case RightBox => addBox(moveTo)
      case _        => return false
  }

  movingBoxes.foreach(loc =>
    setThing(moveDir(loc, dir), getThing(loc))
    setThing(loc, Empty)
  )
  setThing(moveDir(robotLoc, dir), Robot)
  setThing(robotLoc, Empty)
  return true
}
robotLoc

var count = 0
for (direction <- directions) {
  // println(count)
  // println(f"${direction} ${getThing(moveDir(robotLoc, direction))}")

  // printStage()

  val potentialMove = moveDir(robotLoc, direction)
  getThing(potentialMove) match
    case Empty =>
      setThing(robotLoc, Empty)
      setThing(potentialMove, Robot)
      robotLoc = potentialMove
    case LeftBox =>
      direction match
        case Right =>
          var xPos = potentialMove._2
          while (isBox(getThing(potentialMove._1, xPos))) xPos += 1
          if (getThing(potentialMove._1, xPos) == Empty) {
            setThing(robotLoc, Empty)
            setThing(potentialMove, Robot)
            robotLoc = potentialMove
            for (x <- potentialMove._2 + 1 until xPos)
              setThing(
                potentialMove._1,
                x,
                shiftBox(getThing(potentialMove._1, x))
              )
            setThing(potentialMove._1, xPos, RightBox)
          }
        case Left => println("should not move left into left box")
        case Up   => if (shiftUpOrDown(robotLoc, Up)) robotLoc = potentialMove
        case Down =>
          if (shiftUpOrDown(robotLoc, Down)) robotLoc = potentialMove

    case RightBox =>
      direction match
        case Left =>
          var xPos = potentialMove._2
          while (isBox(getThing(potentialMove._1, xPos))) xPos -= 1
          if (getThing(potentialMove._1, xPos) == Empty) {
            setThing(robotLoc, Empty)
            setThing(potentialMove, Robot)
            robotLoc = potentialMove
            for (x <- xPos + 1 until potentialMove._2)
              setThing(
                potentialMove._1,
                x,
                shiftBox(getThing(potentialMove._1, x))
              )
            setThing(potentialMove._1, xPos, LeftBox)
          }
        case Right => println("should not move right into right box")
        case Up    => if (shiftUpOrDown(robotLoc, Up)) robotLoc = potentialMove
        case Down =>
          if (shiftUpOrDown(robotLoc, Down)) robotLoc = potentialMove
    case _ =>
}

printStage()
robotLoc

var sum = 0
for (
  i <- stage.indices;
  j <- stage(i).indices
  if (stage(i)(j) == LeftBox)
) {
  sum += i * 100 + j
}
sum
