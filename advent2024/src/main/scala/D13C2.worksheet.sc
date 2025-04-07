// https://adventofcode.com/2024/day/13
import scala.io.Source
import scala.util.matching.Regex

val filepath = "advent2024/src/main/resources/"
val filename = "D13.txt"
val example = "D13example.txt"

val machines = Source.fromFile(filepath + filename).getLines()
val buttonA: Regex = raw"""Button A: X\+(\d+), Y\+(\d+)""".r
val buttonB: Regex = raw"""Button B: X\+(\d+), Y\+(\d+)""".r
val prize: Regex = raw"""Prize: X=(\d+), Y=(\d+)""".r

object A {
  var x: Long = 0
  var y: Long = 0
  val cost = 3

  def setVars(nx: Long, ny: Long): Unit = {
    x = nx
    y = ny
  }
}

object B {
  var x: Long = 0
  var y: Long = 0
  val cost = 1

  def setVars(nx: Long, ny: Long): Unit = {
    x = nx
    y = ny
  }
}

object Prize {
  var x: Long = 0
  var y: Long = 0

  def setVars(nx: Long, ny: Long): Unit = {
    x = nx + 10000000000000L
    y = ny + 10000000000000L
  }
}

var sum = 0d
for (line <- machines) {
  line match {
    case buttonA(x, y) => A.setVars(x.toInt, y.toInt)
    case buttonB(x, y) => B.setVars(x.toInt, y.toInt)
    case prize(x, y)   => Prize.setVars(x.toInt, y.toInt)
    case _ =>
      val determinant = A.x * B.y - A.y * B.x
      val bPresses: Double = 1d * (A.x * Prize.y - A.y * Prize.x) / determinant
      val aPresses: Double = 1d * (Prize.x * B.y - Prize.y * B.x) / determinant
      println(f"${aPresses}  ${aPresses % 1}")
      println(f"${bPresses}  ${bPresses % 1}")

      if (aPresses % 1 == 0 && bPresses % 1 == 0) {
        sum += B.cost * bPresses + A.cost * aPresses
      }
  }
}
sum.toLong
