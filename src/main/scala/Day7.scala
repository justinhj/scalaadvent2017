import scala.util.matching.Regex

object Day7 {

  case class Program(weight: Int, supporting: List[String])

  def programsFromInput(input : List[String]) : Map[String, Program] = {

    input.map {
      i =>
        val firstSpace = i.indexOf(' ')
        val name = i.substring(0, firstSpace)
        val weight1 = i.indexOf('(')
        val weight2 = i.indexOf(')')
        val weight = i.substring(weight1 + 1, weight2).toInt

        val supported = i.indexOf('>')

        if(supported >= 0) {
          val supportedNames = i.substring(supported + 2, i.size).split("[ ,]+").toList


          (name, Program(weight, supportedNames))

        }
        else {
          (name, Program(weight, List.empty))
        }
    }.toMap

  }

  def main(args : Array[String]) : Unit = {

    val exampleInput =
      """pbga (66)
        |xhth (57)
        |ebii (61)
        |havc (66)
        |ktlj (57)
        |fwft (72) -> ktlj, cntj, xhth
        |qoyq (66)
        |padx (45) -> pbga, havc, qoyq
        |tknk (41) -> ugml, padx, fwft
        |jptl (61)
        |ugml (68) -> gyxo, ebii, jptl
        |gyxo (61)
        |cntj (57)""".stripMargin

    val lines = exampleInput.split('\n').toList

    val programs = programsFromInput(lines)




    var x = 1
    x = 2
  }

}
