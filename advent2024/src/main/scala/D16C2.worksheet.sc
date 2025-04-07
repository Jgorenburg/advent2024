// https://adventofcode.com/2024/day/16
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.mutable.Queue
import scala.collection.mutable.SortedSet
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashSet
// import scala.collection.mutable.Set

val filepath = "advent2024/src/main/resources/"
val filename = "D16.txt"
val example1 = "D16example.txt"
val example2 = "D16example2.txt"
val pathScore =
//   7036 // example1
//   11048 // example2
  78428 // file

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
var onPath = Set[(Int, Int)]()

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
    parents: HashSet[Node],
    g: Int,
    h: Int
) {
  def f: Int = g + h
}

class AStar(stage: Set[(Int, Int)]) {
  // Check if position is within grid bounds and not blocked
  def isValid(coords: (Int, Int)): Boolean =
    !stage.contains(coords)

  def makePath(
      start: Position,
      goal: (Int, Int)
  ): Int = {
    // Priority queue ordered by f-score
    implicit val ordering: Ordering[Node] = Ordering.by[Node, Int](_.f).reverse
    var openSet = PriorityQueue[Node](
      Node(start, HashSet(), 0, start.distance(goal))
    )

    var closedSet = Set.empty[Position]
    var cameFrom = Map.empty[Position, Node]

    while (openSet.nonEmpty) {
      val current = openSet.dequeue()
      val currentpos = current.pos

      closedSet += currentpos

      // Check all valid neighbors
      var nextPositions = Set(
        Position(currentpos.coords, clockwise(currentpos.rotation)),
        Position(currentpos.coords, counterClockwise(currentpos.rotation))
      )
      if (isValid(moveDir(currentpos)))
        nextPositions += Position(moveDir(currentpos), currentpos.rotation)

      nextPositions
        .foreach(nextPos =>
          val tentativeG =
            if nextPos.rotation == currentpos.rotation then current.g + 1
            else current.g + 1000
          val existingNode = cameFrom.get(nextPos)

          if (tentativeG <= pathScore) {
            if (nextPos.coords == target) {
              def addPath(node: Node): Unit = {
                onPath += node.pos.coords
                // println(node.pos.coords)
                node.parents.foreach(addPath(_))
              }

              onPath += nextPos.coords
              addPath(current)
            } else if (
              existingNode.isEmpty || tentativeG < existingNode.get.g
            ) {
              val neighbor = Node(
                pos = nextPos,
                parents = HashSet(current),
                g = tentativeG,
                h = nextPos.distance(goal)
              )

              openSet = openSet.filter(_.pos != nextPos)
              openSet.enqueue(neighbor)
              cameFrom += (nextPos -> neighbor)
            } else if (tentativeG == existingNode.get.g) {
              val neighbor = Node(
                pos = nextPos,
                parents = existingNode.get.parents ++ HashSet(current),
                g = tentativeG,
                h = nextPos.distance(goal)
              )

              openSet = openSet.filter(_.pos != nextPos)
              openSet.enqueue(neighbor)
              cameFrom += (nextPos -> neighbor)
            }
          }
        )
    }

    // No path found
    -1
  }
}

val astar = AStar(stage)
astar.makePath(reindeer, target)
onPath.size
