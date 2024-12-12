// https://adventofcode.com/2024/day/11
import scala.io.Source
import collection.mutable.Map

val filepath = "src/main/resources/"
val filename = "D11.txt"
val example = "D11example.txt"

var initRocks: Map[Long, Long] =
  Source
    .fromFile(filepath + filename)
    .getLines()
    .next()
    .split(" ")
    .map(_.toLong)
    .groupMapReduce(identity)(_ => 1L)(_ + _)
    .to(Map)

val maxDepth = 75

var storedTransforms = Map[Long, List[Long]](0L -> List(1L))
def handleRock(engraved: Long): List[Long] = {
  if (!storedTransforms.contains(engraved)) {

    val sengraved = engraved.toString
    if (sengraved.length() % 2 == 0) {
      storedTransforms(engraved) = List(
        sengraved.take(sengraved.length / 2).toInt,
        sengraved.drop(sengraved.length / 2).toInt
      )
    } else {
      storedTransforms(engraved) = List(engraved * 2024)
    }
  }

  return storedTransforms(engraved)
}

def transform(rocks: Map[Long, Long], depth: Int): Long = {
  if (depth == maxDepth) return rocks.values.sum

  val nextLevel = for ((engraved, repeats) <- rocks.toList) yield {
    (repeats, handleRock(engraved))
  }
  val newRocks = Map[Long, Long]()
  for (
    (size, vals) <- nextLevel;
    v <- vals
  ) {
    if (!newRocks.contains(v)) newRocks(v) = size
    else newRocks(v) += size
  }

  return transform(newRocks, depth + 1)
}

transform(initRocks, 0)
