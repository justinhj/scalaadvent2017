import java.nio.charset.Charset

import better.files.Resource

object Day22 {

  type InitialMap = Vector[Vector[Char]]

  // directions you can face

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction
  case object Up extends Direction
  case object Down extends Direction

  def turnLeft(direction : Direction) : Direction = {
    direction match {
      case Left => Down
      case Up => Left
      case Right => Up
      case Down => Right
    }
  }

  def turnRight(direction : Direction) : Direction = {
    direction match {
      case Left => Up
      case Up => Right
      case Right => Down
      case Down => Left
    }
  }

  case class Coord(row: Int, col: Int)

  type State = Char
  val NormalState : State = '.'
  val InfectedState : State = '#'

  object World {

    def mapFromString(s : String): InitialMap = {

      val rows = s.split('\n').toVector

      rows.map(_.toVector)
    }

    def createMap(map: InitialMap) : Map[Coord, State] = {

      val coordStatePairs = map.zipWithIndex.flatMap {

        case (row, rowIndex) =>

          row.zipWithIndex.map {

            case (state, colIndex) =>
              (Coord(rowIndex, colIndex), state)
          }
      }

      coordStatePairs.foldLeft(Map.empty[Coord, State]){

        case (acc, (k,v)) =>
          acc updated (k,v)
      }

    }

    // Make a world from the string map

    def apply(s: String) : World = {

      // create an initial map which is just an array of char,
      // and we can use that to build a graph of Node
      val initialMap = mapFromString(s)

      val nodes = createMap(initialMap)

      val size = initialMap.head.size

      val start = Coord(size/2, size/2)

      World(nodes, start, Up, 0)
    }

  }

  case class World(nodes : Map[Coord, State], pos: Coord, direction: Direction, causedInfectionCount : Int) {

    // execute a burst of activity and produce a new world

    // If the current node is infected, it turns to its right. Otherwise, it turns to its left. (Turning is done in-place; the
    // current node does not change.)
    // If the current node is clean, it becomes infected. Otherwise, it becomes cleaned. (This is done after the node is
    // considered for the purposes of changing direction.)
    // The virus carrier moves forward one node in the direction it is facing.

    def burst() : World = {

      // We may have moved into the void, and need to create a new node ...

      val currentState = nodes.getOrElse(pos, NormalState)

      val newDirection = if(currentState == InfectedState) turnRight(direction) else turnLeft(direction)

      val newCount = if(currentState == NormalState) causedInfectionCount + 1
                     else causedInfectionCount

      val newState = if(currentState == NormalState) InfectedState else NormalState

      val newPos = newDirection match {
        case Up =>
          Coord(pos.row - 1, pos.col)
        case Down =>
          Coord(pos.row + 1, pos.col)
        case Right =>
          Coord(pos.row, pos.col + 1)
        case Left =>
          Coord(pos.row, pos.col - 1)
      }

      World(nodes updated (pos, newState), newPos, newDirection, newCount)

    }

  }

  def main(args: Array[String]): Unit = {

    val sampleMapStr = """..#
                         |#..
                         |...""".stripMargin

    // make the world from the map

    val sampleWorld = World(sampleMapStr)

    val sampleAnswer = (1 to 10000).foldLeft(sampleWorld) {

      case (world, step) =>

        val newWorld = world.burst()
        //println(s"$step: ${newWorld.causedInfectionCount}")
        newWorld

    }

    println(s"sample answer ${sampleAnswer.causedInfectionCount}")

    val step1MapStr = Resource.getAsString("input22.txt")(Charset.forName("US-ASCII"))

    var step1World = World(step1MapStr)

    val step1Answer = (1 to 10000).foldLeft(step1World) {

      case (world, step) =>

        val newWorld = world.burst()
        //println(s"$step: ${newWorld.causedInfectionCount}")
        newWorld

    }

    println(s"step1 answer ${step1Answer.causedInfectionCount}")


  }

}

/*
Diagnostics indicate that the local grid computing cluster has been contaminated with the Sporifica Virus. The grid
computing cluster is a seemingly-infinite two-dimensional grid of compute nodes. Each node is either clean or infected
by the virus.

To prevent overloading the nodes (which would render them useless to the virus) or detection by system administrators,
exactly one virus carrier moves through the network, infecting or cleaning nodes as it moves. The virus carrier is always
located on a single node in the network (the current node) and keeps track of the direction it is facing.

To avoid detection, the virus carrier works in bursts; in each burst, it wakes up, does some work, and goes back to sleep.
The following steps are all executed in order one time each burst:

If the current node is infected, it turns to its right. Otherwise, it turns to its left. (Turning is done in-place; the
current node does not change.)
If the current node is clean, it becomes infected. Otherwise, it becomes cleaned. (This is done after the node is
considered for the purposes of changing direction.)
The virus carrier moves forward one node in the direction it is facing.
Diagnostics have also provided a map of the node infection status (your puzzle input). Clean nodes are shown as .;
infected nodes are shown as #. This map only shows the center of the grid; there are many more nodes beyond those shown,
but none of them are currently infected.

The virus carrier begins in the middle of the map facing up.

For example, suppose you are given a map like this:

..#
#..
...
Then, the middle of the infinite grid looks like this, with the virus carrier's position marked with [ ]:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . . #[.]. . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
The virus carrier is on a clean node, so it turns left, infects the node, and moves left:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . .[#]# . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
The virus carrier is on an infected node, so it turns right, cleans the node, and moves up:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . .[.]. # . . .
. . . . # . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
Four times in a row, the virus carrier finds a clean, infects it, turns left, and moves forward, ending in the same
place and still facing up:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . #[#]. # . . .
. . # # # . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
Now on the same node as before, it sees an infection, which causes it to turn right, clean the node, and move forward:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . # .[.]# . . .
. . # # # . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
After the above actions, a total of 7 bursts of activity had taken place. Of them, 5 bursts of activity caused an
infection.

After a total of 70, the grid looks like this, with the virus carrier facing up:

. . . . . # # . .
. . . . # . . # .
. . . # . . . . #
. . # . #[.]. . #
. . # . # . . # .
. . . . . # # . .
. . . . . . . . .
. . . . . . . . .
By this time, 41 bursts of activity caused an infection (though most of those nodes have since been cleaned).

After a total of 10000 bursts of activity, 5587 bursts will have caused an infection.

Given your actual map, after 10000 bursts of activity, how many bursts cause a node to become infected? (Do not count
nodes that begin infected.)

Step 2

--- Part Two ---
As you go to remove the virus from the infected nodes, it evolves to resist your attempt.

Now, before it infects a clean node, it will weaken it to disable your defenses. If it encounters an infected node, it
will instead flag the node to be cleaned in the future. So:

Clean nodes become weakened.
Weakened nodes become infected.
Infected nodes become flagged.
Flagged nodes become clean.
Every node is always in exactly one of the above states.

The virus carrier still functions in a similar way, but now uses the following logic during its bursts of action:

Decide which way to turn based on the current node:
If it is clean, it turns left.
If it is weakened, it does not turn, and will continue moving in the same direction.
If it is infected, it turns right.
If it is flagged, it reverses direction, and will go back the way it came.
Modify the state of the current node, as described above.
The virus carrier moves forward one node in the direction it is facing.
Start with the same map (still using . for clean and # for infected) and still with the virus carrier starting in the
middle and facing up.

Using the same initial state as the previous example, and drawing weakened as W and flagged as F, the middle of the
infinite grid looks like this, with the virus carrier's position again marked with [ ]:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . . #[.]. . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
This is the same as before, since no initial nodes are weakened or flagged. The virus carrier is on a clean node, so
it still turns left, instead weakens the node, and moves left:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . .[#]W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
The virus carrier is on an infected node, so it still turns right, instead flags the node, and moves up:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . .[.]. # . . .
. . . F W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
This process repeats three more times, ending on the previously-flagged node and facing right:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . W W . # . . .
. . W[F]W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
Finding a flagged node, it reverses direction and cleans the node:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . W W . # . . .
. .[W]. W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
The weakened node becomes infected, and it continues in the same direction:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . W W . # . . .
.[.]# . W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
Of the first 100 bursts, 26 will result in infection. Unfortunately, another feature of this evolved virus is speed;
of the first 10000000 bursts, 2511944 will result in infection.

Given your actual map, after 10000000 bursts of activity, how many bursts cause a node to become infected? (Do not
count nodes that begin infected.)




 */