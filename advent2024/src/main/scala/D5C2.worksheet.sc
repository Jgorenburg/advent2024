import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.Set

val filepath = "src/main/resources/"
val filename = "D5.txt"

val rule = raw"([0-9]+)\|([0-9]+)".r
val orderRules: Map[String, Set[String]] = Map()

var badUpdates = 0

for (line <- Source.fromFile(filepath + filename).getLines) {
  line match
    case rule(lead, follow) =>
      if (!orderRules.contains(follow)) orderRules(follow) = Set()
      if (!orderRules.contains(lead)) orderRules(lead) = Set(follow)
      else orderRules(lead) += follow
    case "" =>
    case _ =>
      val updateOrder = line.split(",")
      var soFar: Set[String] = Set()
      if (
        updateOrder.exists(page =>
          soFar += page
          (soFar & orderRules(page)).nonEmpty
        )
      )
        badUpdates += (updateOrder
          .find(page =>
            (updateOrder.toSet & orderRules(page)).size == updateOrder.size / 2
          ) match
          case Some(s) => s.toInt
          case None    => 0
        )
}

badUpdates
