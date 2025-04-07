// https://adventofcode.com/2024/day/21
import scala.util.matching.Regex
import scala.io.Source
import collection.mutable.Map

val filepath = "advent2024/src/main/resources/"
val filename = "D21.txt"
val example = "D21example.txt"
val input = Source.fromFile(filepath + filename).getLines()

object Instruction extends Enumeration {
  type Instruction = Value
  val Left, Right, Up, Down, Activate = Value
}

import Instruction.*

val numpad = List('A', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

val numpadLoc = Map(
  '7' -> (0, 0),
  '8' -> (0, 1),
  '9' -> (0, 2),
  '4' -> (1, 0),
  '5' -> (1, 1),
  '6' -> (1, 2),
  '1' -> (2, 0),
  '2' -> (2, 1),
  '3' -> (2, 2),
  '0' -> (3, 1),
  'A' -> (3, 2)
)

val arrowpadLoc = Map(
  Up -> (0, 1),
  Activate -> (0, 2),
  Left -> (1, 0),
  Down -> (1, 1),
  Right -> (1, 2)
)

def getInstructions(start: (Int, Int), end: (Int, Int)): List[Instruction] = {
  val x = start._2 - end._2
  val y = start._1 - end._1

  var instructs = List[Instruction]()

  if (y > 0) {
    instructs ++= List.fill(y)(Up)
  } else if (y < 0) {
    instructs ++= List.fill(-y)(Down)
  }
  if (x > 0) {
    instructs ++= List.fill(x)(Left)
  } else if (x < 0) {
    instructs ++= List.fill(-x)(Right)
  }

  instructs
}

def uniquePermutations(
    list: List[Instruction],
    isNumpad: Boolean,
    startloc: (Int, Int)
): List[List[Instruction]] = {
  // Base cases
  if (list.isEmpty) List(List(Activate))
  else {
    // Group elements by their value and count occurrences
    val grouped = list.groupBy(identity).view.mapValues(_.size).toList

    def generate(
        remaining: List[(Instruction, Int)],
        loc: (Int, Int)
    ): List[List[Instruction]] = {
      if (remaining.isEmpty) List(List(Activate))
      else {
        for {
          // For each possible element we can put first
          (elem, count) <- remaining
          // Get remaining elements after using this one
          nextRemaining = remaining
            .map {
              case (e, c) if e == elem && c == 1 => (e, c - 1)
              case (e, c) if e == elem           => (e, c - 1)
              case other                         => other
            }
            .filter(_._2 > 0)

          // Do not leave the keypad
          nextLoc = elem match {
            case Left     => (loc._1, loc._2 - 1)
            case Down     => (loc._1 + 1, loc._2)
            case Right    => (loc._1, loc._2 + 1)
            case Up       => (loc._1 - 1, loc._2)
            case Activate => (loc._1, loc._2)
          }
          if ((isNumpad && nextLoc != (3, 0)) || (!isNumpad && nextLoc != (0, 0)))

          // Recursively get permutations of remaining elements
          rest <- generate(nextRemaining, nextLoc)
        } yield elem :: rest
      }
    }

    generate(grouped, startloc)
  }
}

var numpadDist = Map[(Char, Char), Int]()
var arrowpadDist = Array.fill(3)(Map[(Instruction, Instruction), Int]())

def getPathLen(path: List[Instruction], depth: Int): Int = {
  var prevInstruct = Activate
  var pathlen = 0
  for (instruct <- path) {
    pathlen += arrowpadDist(depth - 1)((prevInstruct, instruct))
    prevInstruct = instruct
  }
  return pathlen
}

Instruction.values.foreach(start =>
  Instruction.values.foreach(end => arrowpadDist(0)((start, end)) = 1)
)

Instruction.values.foreach(start =>
  Instruction.values.foreach(end =>
    println(f"${start} ${end}")
    println(
      uniquePermutations(
        getInstructions(arrowpadLoc(start), arrowpadLoc(end)),
        false,
        arrowpadLoc(start)
      )
    )
    println(
      uniquePermutations(
        getInstructions(arrowpadLoc(start), arrowpadLoc(end)),
        false,
        arrowpadLoc(start)
      ).map(getPathLen(_, 1)).min
    )
    arrowpadDist(1)((start, end)) = uniquePermutations(
      getInstructions(arrowpadLoc(start), arrowpadLoc(end)),
      false,
      arrowpadLoc(start)
    ).map(getPathLen(_, 1)).min
  )
)

Instruction.values.foreach(start =>
  Instruction.values.foreach(end =>
    arrowpadDist(2)((start, end)) = uniquePermutations(
      getInstructions(arrowpadLoc(start), arrowpadLoc(end)),
      false,
      arrowpadLoc(start)
    ).map(getPathLen(_, 2)).min
    println(f"${start} ${end}")
    println(
      uniquePermutations(
        getInstructions(arrowpadLoc(start), arrowpadLoc(end)),
        false,
        arrowpadLoc(start)
      )
    )
    println(arrowpadDist(2)((start, end)))
  )
)

numpad.foreach(start =>
  numpad.foreach(end =>
    numpadDist((start, end)) = uniquePermutations(
      getInstructions(numpadLoc(start), numpadLoc(end)),
      true,
      numpadLoc(start)
    ).map(getPathLen(_, 3)).min
    println(f"${start} ${end}")
    println(
      uniquePermutations(
        getInstructions(numpadLoc(start), numpadLoc(end)),
        true,
        numpadLoc(start)
      )
    )
    println(numpadDist(start, end))
  )
)

var totalComplexity = 0
for (line <- input) {

  var prevButton = 'A'
  var pressed = 0
  for (button <- line) {
    pressed += numpadDist(prevButton, button)
    prevButton = button
  }

  totalComplexity += pressed * line.filter(_.isDigit).toInt
  println(
    f"${pressed} ${line.filter(_.isDigit).toInt} ${pressed * line.filter(_.isDigit).toInt}"
  )
}

totalComplexity
