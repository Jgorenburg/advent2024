// https://adventofcode.com/2024/day/3
import scala.util.matching.Regex
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D3.txt"
val input = Source.fromFile(filepath + filename).mkString
val mulRegex: Regex = """mul\(([0-9]+),([0-9]+)\)""".r

mulRegex
  .findAllIn(input)
  .matchData
  .map(m => m.group(1).toInt * m.group(2).toInt)
  .sum
