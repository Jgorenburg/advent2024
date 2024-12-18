// https://adventofcode.com/2024/day/18
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D18.txt"
val example1 = "D18example.txt"

val realInput = Source.fromFile(filepath + filename).getLines().toArray
val exampleInput = Source.fromFile(filepath + example1).getLines().toArray

val exampleWalls = (for (line <- 0 until 12) yield {
  val pos = exampleInput(line).split(",").map(_.toInt)
  Position(pos(0), pos(1))
}).toSet

val realWalls = (for (line <- 0 until 1024) yield {
  val pos = realInput(line).split(",").map(_.toInt)
  Position(pos(0), pos(1))
}).toSet

case class Position(x: Int, y: Int) {
  def manhattan(other: Position): Int =
    Math.abs(x - other.x) + Math.abs(y - other.y)

  def neighbors: List[Position] = List(
    Position(x - 1, y),
    Position(x + 1, y),
    Position(x, y - 1),
    Position(x, y + 1)
  )
}

case class Node(
    pos: Position,
    parent: Option[Node],
    g: Int,
    h: Int
) {
  def f: Int = g + h
}

class AStar(width: Int, height: Int, stage: Set[Position]) {
  // Check if position is within grid bounds and not blocked
  def isValid(pos: Position): Boolean =
    pos.x >= 0 && pos.x < width &&
      pos.y >= 0 && pos.y < height &&
      !stage.contains(pos)

  def findPath(
      start: Position,
      goal: Position
  ): Int = {
    // Priority queue ordered by f-score
    implicit val ordering: Ordering[Node] = Ordering.by[Node, Int](_.f).reverse
    var openSet = collection.mutable.PriorityQueue[Node](
      Node(start, None, 0, start.manhattan(goal))
    )

    var closedSet = Set.empty[Position]
    var cameFrom = Map.empty[Position, Node]

    while (openSet.nonEmpty) {
      val current = openSet.dequeue()
      println(current)

      if (current.pos == goal) {
        def reconstructPath(node: Node): Unit =
          node.parent match {
            case Some(parent) =>
              println(parent.pos)
              reconstructPath(parent)
            case _ => List()
          }
        reconstructPath(current)
        return current.g
      }

      closedSet += current.pos

      // Check all valid neighbors
      for {
        nextPos <- current.pos.neighbors
        if (isValid(nextPos) && !closedSet.contains(nextPos))
      } {
        val tentativeG = current.g + 1
        val existingNode = cameFrom.get(nextPos)

        if (existingNode.isEmpty || tentativeG < existingNode.get.g) {
          val neighbor = Node(
            pos = nextPos,
            parent = Some(current),
            g = tentativeG,
            h = nextPos.manhattan(goal)
          )

          openSet = openSet.filter(_.pos != nextPos)
          openSet.enqueue(neighbor)
          cameFrom += (nextPos -> neighbor)
        }
      }
    }

    println(closedSet)
    // No path found
    -1
  }
}

val exAStar = AStar(7, 7, exampleWalls)
exAStar.findPath(Position(0, 0), Position(6, 6))

val aStar = AStar(71, 71, realWalls)
// aStar.findPath(Position(0, 0), Position(70, 70))
