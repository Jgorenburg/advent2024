// https://adventofcode.com/2024/day/22
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D22.txt"
val example = "D22example.txt"
val input = Source.fromFile(filepath + filename).getLines()

def mix(v1: Long, v2: Long): Long = v1 ^ v2

def prune(sn: Long): Long = sn % 16777216

var sum = 0L
for (line <- input) {
  var secretNum = line.toLong
  for (_ <- 0 until 2000) {
    secretNum = mix(secretNum * 64, secretNum)
    secretNum = prune(secretNum)
    secretNum = mix(secretNum / 32, secretNum)
    secretNum = prune(secretNum)
    secretNum = mix(secretNum * 2048, secretNum)
    secretNum = prune(secretNum)
    println(secretNum)
  }
  sum += secretNum
}
sum
