// https://adventofcode.com/2024/day/22
import scala.io.Source
import scala.collection.mutable.{Map, Queue}

val filepath = "advent2024/src/main/resources/"
val filename = "D22.txt"
val example2 = "D22example2.txt"
val input = Source.fromFile(filepath + filename).getLines()

def mix(v1: Long, v2: Long): Long = v1 ^ v2

def prune(sn: Long): Long = sn % 16777216

var sums = Map[Queue[Long], Long]()
for (line <- input) {
  var secretNum = line.toLong
  var bananaCount = secretNum % 10
  var lastFour = Queue[Long]()
  var buys = Map[Queue[Long], Long]()

  for (_ <- 0 until 2000) {
    secretNum = mix(secretNum * 64, secretNum)
    secretNum = prune(secretNum)
    secretNum = mix(secretNum / 32, secretNum)
    secretNum = prune(secretNum)
    secretNum = mix(secretNum * 2048, secretNum)
    secretNum = prune(secretNum)
    lastFour.enqueue((secretNum % 10) - bananaCount)
    if (lastFour.length > 3) {
      if (!buys.contains(lastFour)) {
        buys(Queue(lastFour.toSeq*)) = secretNum % 10
      }
      lastFour.dequeue()
    }
    println(lastFour)
    bananaCount = secretNum % 10
  }

  buys.foreach((k, v) =>
    if (sums contains k) {
      sums(k) += v
    } else {
      sums(k) = v
    }
  )
}
sums
sums.values.max
