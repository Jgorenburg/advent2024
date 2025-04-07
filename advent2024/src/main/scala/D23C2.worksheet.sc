// https://adventofcode.com/2024/day/23
import scala.io.Source
import collection.mutable.{Map, Set}

val filepath = "advent2024/src/main/resources/"
val filename = "D23.txt"
val example = "D23example.txt"
val input = Source.fromFile(filepath + filename).getLines()

var connections = Map[String, Set[String]]()
var networks = Set[Set[String]](Set())

def sameConnections(c1: String, c2: String): Boolean = {
  connections(c1).toSet + c1 == connections(c2).toSet + c2
}

for (line <- input) {
  val Array(c1: String, c2: String) = line.split("-", 2)
  connections.getOrElseUpdate(c1, Set()) += c2
  connections.getOrElseUpdate(c2, Set()) += c1
  networks
    .filter(network => network.forall(connections(_) contains c1))
    .foreach(network => networks += network union Set(c1))
  networks
    .filter(network => network.forall(connections(_) contains c2))
    .foreach(network => networks += network union Set(c2))
}

networks.maxBy(_.size).toSeq.sorted.mkString(",")
