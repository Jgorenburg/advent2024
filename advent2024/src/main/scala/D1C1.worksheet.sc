// https://adventofcode.com/2024/day/1
import scala.io.Source
import scala.collection.mutable.PriorityQueue

val filepath = "src/main/resources/"
val filename = "D1.txt"
var firstL: PriorityQueue[Int] = PriorityQueue()
var secondL: PriorityQueue[Int] = PriorityQueue()
for (line <- Source.fromFile(filepath + filename).getLines.toList)
  val split = line.split("   ")
  firstL.enqueue(split(0).toInt)
  secondL.enqueue(split(1).toInt)

var diff = 0
while (!firstL.isEmpty) {
  diff += Math.abs(firstL.dequeue() - secondL.dequeue())
}
diff
