import java.nio.charset.Charset
import better.files.Resource
import scala.util.Try

object Day19 {

  import scalaz._, Scalaz._

  // Network can simply be Vector of Vectors

  type Network = Vector[Vector[Char]]

  val sampleNetwork = """
              |     |
              |     |  +--+
              |     A  |  C
              | F---|----E|--+
              |     |  |  |  D
              |     +B-+  +--+
""".stripMargin

  def networkFromString(input : String) : Network = {

    val rows : Vector[String] = input.split("\n").toVector

    rows.map {
      row =>
        row.toVector
    }

  }

  case class Location(row: Int, col: Int)

  def getStartPosition(network: Network) : Option[Location] = {

    // Handle that the first line is empty

    val startLine = if(network(0).size == 0) 1 else 0

    // Find the first down line

    val index = network(startLine).indexOf('|')

    if(index > 0) Some(Location(startLine, index))
    else None
  }

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction
  case object Up extends Direction
  case object Down extends Direction

  // give a direction and location return the next location
  def move(direction: Direction, location: Location) : Location = {

    direction match {
      case Up => location.copy(row = location.row - 1)
      case Down => location.copy(row = location.row + 1)
      case Left => location.copy(col = location.col - 1)
      case Right => location.copy(col = location.col + 1)
    }

  }


  def getCharAtPos(network: Network, pos: Location) = {
    Try{network(pos.row)(pos.col)}.getOrElse(' ')
  }

  def canMoveTo(char: Char) = {

    char match {
      case '|' => true
      case '-' => true
      case letter : Char if letter >= 'A' && letter <= 'Z' => true

      case _ => false
    }

  }

  // Walk the network and when you reach the end return the letters seen
  def walkNetwork(network: Network, curPos: Location, curDir: Direction, letters: String, steps: Int) : (String, Int) = {

    // What happens on each step is based on what is there

    val tile = getCharAtPos(network, curPos)

    tile match {

      // Direction indicators mean just keep going
      case '|' =>
        walkNetwork(network, move(curDir, curPos), curDir, letters, steps + 1)
      case '-' =>
        walkNetwork(network, move(curDir, curPos), curDir, letters, steps + 1)

      // In the case of a letter keep going but store the letter
      case letter : Char if letter >= 'A' && letter <= 'Z' =>
        walkNetwork(network, move(curDir, curPos), curDir, letter +: letters, steps + 1)

      // We blundered into empty space so that's the end
      case ' ' =>
        (letters.reverse, steps)

      // This is the only tricky one because we must take a different
      // direction depending on what's around ...
      case '+' =>
        if(curDir == Down || curDir == Up) {
          val moveLeft = move(Left, curPos)
          val whatsLeft = getCharAtPos(network, moveLeft)

          if(canMoveTo(whatsLeft))
            walkNetwork(network, moveLeft, Left, letters, steps + 1)
          else
            walkNetwork(network, move(Right, curPos), Right, letters, steps + 1)

        }
        else {
          val moveUp = move(Up, curPos)
          val whatsUp = getCharAtPos(network, moveUp)

          if(canMoveTo(whatsUp))
            walkNetwork(network, moveUp, Up, letters, steps + 1)
          else
            walkNetwork(network, move(Down, curPos), Down, letters, steps + 1)

        }

    }

  }

  def main(args: Array[String]) : Unit = {

    // Sample

    val network = networkFromString(sampleNetwork)

    getStartPosition(network).map {
      start =>
        val (result, steps) = walkNetwork(network, start, Down, "", 0)
        println(s"Sample result $result $steps")

    }

    // Step 1

    val step1SampleNetwork = Resource.getAsString("input19.txt")(Charset.forName("US-ASCII"))

    val step1Network = networkFromString(step1SampleNetwork)

    getStartPosition(step1Network).map {
      start =>
        val (result, steps) = walkNetwork(step1Network, start, Down, "", 0)
        println(s"Step1 result $result $steps")

    }


  }


}

/*
--- Day 19: A Series of Tubes ---
Somehow, a network packet got lost and ended up here. It's trying to follow a routing diagram (your puzzle input), but
 it's confused about where to go.

Its starting point is just off the top of the diagram. Lines (drawn with |, -, and +) show the path it needs to take,
starting by going down onto the only line connected to the top of the diagram. It needs to follow this path until it
reaches the end (located somewhere within the diagram) and stop there.

Sometimes, the lines cross over each other; in these cases, it needs to continue going the same direction, and only
 turn left or right when there's no other option. In addition, someone has left letters on the line; these also don't
 change its direction, but it can use them to keep track of where it's been. For example:

     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+

Given this diagram, the packet needs to take the following path:

Starting at the only line touching the top of the diagram, it must go down, pass through A, and continue onward to the
first +.
Travel right, up, and right, passing through B in the process.
Continue down (collecting C), right, and up (collecting D).
Finally, go all the way left through E and stopping at F.
Following the path to the end, the letters it sees on its path are ABCDEF.

The little packet looks up at you, hoping you can help it find the way. What letters will it see (in the order it would
see them) if it follows the path? (The routing diagram is very wide; make sure you view it without line wrapping.)
 */
