import scala.io.Source

object Day12 {

  val inputLines = Source.fromResource("input12.txt").getLines.toList

  val testInput = """0 <-> 2
                    |1 <-> 1
                    |2 <-> 0, 3, 4
                    |3 <-> 2, 4
                    |4 <-> 2, 3, 6
                    |5 <-> 6
                    |6 <-> 4, 5""".stripMargin

  val testInputLines = testInput.lines.toList

  def parseLine(line: String) : (Int, Set[Int]) = {

   //  151 <-> 542, 995, 1027

    val pattern = """([-]*\d+) <-> ([0-9, ]+)""".r

    val pattern(id, connections) = line

    val parsedConnections = connections.split("[, ]+").map(_.toInt)

    (id.toInt, parsedConnections.toSet)
  }

  def parseInput(lines: List[String]) : Map[Int, Set[Int]] = {

    lines.map(parseLine).toMap
  }

  // Recursively find connections to program id, keep track of explored connections
  def collectConnections(id: Int, programs : Map[Int, Set[Int]], connections: Set[Int]) : Set[Int] = {

    if(connections contains id) connections
    else {

      // add this to the connections and explore all this nodes connections

      programs.get(id).get.foldLeft(connections + id) {

        case (acc, c) =>
          acc ++ collectConnections(c, programs, acc)
      }

    }

  }

  def countGroups(programs: Map[Int, Set[Int]], groupCount : Int = 0) : Int = {

    if(programs.isEmpty) groupCount
    else {

      val firstProgram = programs.head

      val connections = collectConnections(firstProgram._1, programs, Set.empty)

      // remove these program and repeat

      countGroups(programs -- connections, groupCount + 1)

    }

  }

  def main(args: Array[String]) : Unit = {

    // Step 1 is simple, we just recursively determine group 0 and output the size

    val step1Input = parseInput(inputLines)
    val testInput = parseInput(testInputLines)

    assert(collectConnections(0, testInput, Set.empty).size == 6)
    val collectedStep1Connections = collectConnections(0, step1Input, Set.empty)

    println(s"Step 1 answer : ${collectedStep1Connections.size}")

    // Step 2 requires us to count unique groups

    // To do that we'll simply repeat step one with the first program in the programs list, then remove those
    // programs. We can repeat until there are no programs, and count how many iterations we did

    assert(countGroups(testInput) == 2)

    println(s"Step 2 answer : ${countGroups(step1Input)}")

  }


}
