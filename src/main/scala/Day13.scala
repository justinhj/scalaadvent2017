import scala.io.Source

object Day13 {

  val testInput = """0: 3
                    |1: 2
                    |4: 4
                    |6: 4""".stripMargin

  val testInputLines = testInput.lines.toList

  case class Layer(layer: Int, range: Int)

  def parseLine(line: String) : Layer = {

    val pattern = """([-]*\d+): ([-]*\d+)""".r

    val pattern(layer, range) = line

    Layer(layer.toInt, range.toInt)
  }

  def parseInput(lines: List[String]) : List[Layer] = {

    lines.map(parseLine)
  }

  def main(args: Array[String]) : Unit = {

    // Step 1 is simple, we just recursively determine group 0 and output the size

    val step1Input = parseInput(step1InputLines)
    val testInput = parseInput(testInputLines)


    var x = 1

  }

  val step1InputLines = """0: 3
                     |1: 2
                     |2: 4
                     |4: 4
                     |6: 5
                     |8: 6
                     |10: 6
                     |12: 6
                     |14: 6
                     |16: 8
                     |18: 8
                     |20: 8
                     |22: 8
                     |24: 10
                     |26: 8
                     |28: 8
                     |30: 12
                     |32: 14
                     |34: 12
                     |36: 10
                     |38: 12
                     |40: 12
                     |42: 9
                     |44: 12
                     |46: 12
                     |48: 12
                     |50: 12
                     |52: 14
                     |54: 14
                     |56: 14
                     |58: 12
                     |60: 14
                     |62: 14
                     |64: 12
                     |66: 14
                     |70: 14
                     |72: 14
                     |74: 14
                     |76: 14
                     |80: 18
                     |88: 20
                     |90: 14
                     |98: 17""".stripMargin.lines.toList


}
