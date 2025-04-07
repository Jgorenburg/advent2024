// https://adventofcode.com/2024/day/16
import scala.io.Source
import scala.collection.mutable.PriorityQueue

val filepath = "advent2024/src/main/resources/"
val filename = "D16.txt"
val example1 = "D16example.txt"
val example2 = "D16example2.txt"

val input = Source.fromFile(filepath + filename).getLines().toArray

object Direction extends Enumeration {
  type Direction = Value
  val Up, Left, Down, Right = Value

  def moveDir(loc: (Int, Int), dir: Direction): (Int, Int) = {
    val offset = dir match
      case Up    => (-1, 0)
      case Down  => (1, 0)
      case Right => (0, 1)
      case Left  => (0, -1)
    return (loc._1 + offset._1, loc._2 + offset._2)
  }

  def moveDir(pos: Position): (Int, Int) = moveDir(pos.coords, pos.rotation)

  def clockwise(dir: Direction): Direction = {
    dir match
      case Up    => Right
      case Right => Down
      case Down  => Left
      case Left  => Up
  }

  def counterClockwise(dir: Direction): Direction = {
    dir match
      case Up    => Left
      case Left  => Down
      case Down  => Right
      case Right => Up
  }
}

import Direction.*
case class Position(coords: (Int, Int), rotation: Direction) {
  def distance(other: (Int, Int)): Int =
    Math.abs(coords._1 - other._1) + Math.abs(coords._2 - other._2)

  def neighbors: List[Position] = List(
    Position(moveDir(coords, rotation), rotation),
    Position(coords, clockwise(rotation)),
    Position(coords, counterClockwise(rotation))
  )
}

var stage = Set[(Int, Int)]()
var reindeer = Position((0, 0), Right)
var target = (0, 0)

for (
  i <- input.indices;
  j <- input(0).indices;
  if (input(i)(j) != '.')
) {
  if (input(i)(j) == 'E') target = (i, j)
  else if (input(i)(j) == 'S') reindeer = Position((i, j), Right)
  else stage += (i, j)
}
target
reindeer
stage

case class Node(
    pos: Position,
    g: Int,
    h: Int
) {
  def f: Int = g + h
}

class AStar(stage: Set[(Int, Int)]) {
  def isValid(coords: (Int, Int)): Boolean =
    !stage.contains(coords)

  def makePath(
      start: Position,
      goal: (Int, Int)
  ): Int = {
    // Priority queue ordered by f-score
    implicit val ordering: Ordering[Node] = Ordering.by[Node, Int](_.f).reverse
    var openSet = PriorityQueue[Node](
      Node(start, 0, start.distance(goal))
    )

    var closedSet = Set.empty[Position]
    var cameFrom = Map.empty[Position, Node]

    while (openSet.nonEmpty) {
      val current = openSet.dequeue()
      val currentpos = current.pos

      if (currentpos.coords == goal) {
        return current.g
      }

      closedSet += currentpos

      // Check all valid neighbors
      var nextPositions = Set(
        Position(currentpos.coords, clockwise(currentpos.rotation)),
        Position(currentpos.coords, counterClockwise(currentpos.rotation))
      )
      if (isValid(moveDir(currentpos)))
        nextPositions += Position(moveDir(currentpos), currentpos.rotation)

      nextPositions
        .filterNot(pos => closedSet.contains(pos))
        .foreach(nextPos =>
          val tentativeG =
            if nextPos.rotation == currentpos.rotation then current.g + 1
            else current.g + 1000
          val existingNode = cameFrom.get(nextPos)

          if (existingNode.isEmpty || tentativeG < existingNode.get.g) {
            val neighbor = Node(
              pos = nextPos,
              g = tentativeG,
              h = nextPos.distance(goal)
            )

            openSet = openSet.filter(_.pos != nextPos)
            openSet.enqueue(neighbor)
            cameFrom += (nextPos -> neighbor)
          }
        )
    }

    // No path found
    return -1
  }
}

val astar = AStar(stage)
astar.makePath(reindeer, target)
