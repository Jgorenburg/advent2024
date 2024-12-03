import scala.util.matching.Regex
import scala.io.Source

val filepath = "src/main/resources/"
val filename = "D3.txt"
val input = Source.fromFile(filepath + filename).mkString
val mulRegex: Regex = """mul\(([0-9]+),([0-9]+)\)""".r
val donts = input.split("don't()")

(mulRegex
  .findAllIn(donts(0))
  .matchData
  .map(m => m.group(1).toInt * m.group(2).toInt)
  .sum)
  +
    (donts
      .drop(1)
      .map(frag =>
        frag.indexOf("do()") match {
          case -1 => 0
          case x =>
            mulRegex
              .findAllIn(frag.splitAt(x)(1))
              .matchData
              .map(m => m.group(1).toInt * m.group(2).toInt)
              .sum
        }
      )
      .sum)
