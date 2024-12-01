import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Map

val filepath = "src/main/resources/"
val filename = "D1.txt"
var firstL: Queue[Int] = Queue()
var secondL: Map[Int, Int] = Map()
for (line <- Source.fromFile(filepath + filename).getLines.toList)
  val split = line.split("   ")
  firstL.enqueue(split(0).toInt)
  if (secondL contains split(1).toInt) secondL(split(1).toInt) += 1
  else secondL(split(1).toInt) = 1

var sim = 0
while (!firstL.isEmpty) {
  val num = firstL.dequeue()
  if (secondL contains num) sim += secondL(num) * num
}
sim
