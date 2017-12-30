import scala.io.Source

object Day12 {

  val inputLines = Source.fromResource("input12.txt").getLines.toList

  def parseLine(line: String) : (Int, Set[Int]) = {

   //  151 <-> 542, 995, 1027

    val pattern = """([-]*\d+) <-> ([0-9, ]+)""".r

    val pattern(id, connections) = line

    val parsedConnections = connections.split("[, ]+").map(_.toInt)

    (id.toInt, parsedConnections.toSet)
  }

  def parseInput(lines: List[String]) : Map[Int, Set[Int]] = {

    val parsed = lines.map(parseLine)


    ???
  }

  def main(args: Array[String]) : Unit = {

    println("hello")

    val p = parseInput(inputLines)

    var x = 1
    x = 2



  }


}
