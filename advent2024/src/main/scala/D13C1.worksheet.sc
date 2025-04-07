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
  var x: Int = 0
  var y: Int = 0
  val cost = 3

  def setVars(nx: Int, ny: Int): Unit = {
    x = nx
    y = ny
  }
}

object B {
  var x: Int = 0
  var y: Int = 0
  val cost = 1

  def setVars(nx: Int, ny: Int): Unit = {
    x = nx
    y = ny
  }
}

object Prize {
  var x: Int = 0
  var y: Int = 0

  def setVars(nx: Int, ny: Int): Unit = {
    x = nx
    y = ny
  }
}

var sum = 0
for (line <- machines) {
  line match {
    case buttonA(x, y) => A.setVars(x.toInt, y.toInt)
    case buttonB(x, y) => B.setVars(x.toInt, y.toInt)
    case prize(x, y)   => Prize.setVars(x.toInt, y.toInt)
    case _ =>
      var aPresses = 0
      var bPresses = 0
      while (
        bPresses <= 100 & bPresses * B.x <= Prize.x && bPresses * B.y <= Prize.y
      ) bPresses += 1
      if (bPresses * B.x == Prize.x && bPresses * B.y == Prize.y)
        sum += bPresses * B.cost
      else {
        while (bPresses >= 0 && aPresses <= 100) {
          if (
            aPresses * A.x + bPresses * B.x == Prize.x && aPresses * A.y + bPresses * B.y == Prize.y
          ) {
            sum += aPresses * A.cost + bPresses * B.cost
            bPresses = -1
          } else if (
            aPresses * A.x + bPresses * B.x > Prize.x || aPresses * A.y + bPresses * B.y > Prize.y
          ) bPresses -= 1
          else aPresses += 1
        }
      }
  }
}
sum
