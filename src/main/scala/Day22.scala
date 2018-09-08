import java.nio.charset.Charset

import better.files.Resource

object Day22 {

  type Map = Vector[Vector[Char]]

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

  sealed trait State
  case object Normal extends State
  case object Infected extends State

  case class Node(var state : State = Normal,
                  var above: Option[Node] = None,
                  var below: Option[Node] = None,
                  var left: Option[Node] = None,
                  var right: Option[Node] = None
                 )

  def printMap(map: Map) : Unit = {

//    map.foreach {
//      r =>
//        println(r.mkString)
//
//    }

  }

  object World {

    def mapFromString(s : String): Map = {

      val rows = s.split('\n').toVector

      rows.map(_.toVector)
    }

    def linkNodes(nodes: Vector[Vector[Node]]) : Unit = {
      // we assume square
      val size = nodes.head.size

      nodes.zipWithIndex.foreach {

        case (row, rowNumber) =>

          row.zipWithIndex.foreach {

            case (node, colNumber) =>

              // above links
              if(rowNumber > 0 ) {
                node.above = Some(nodes(rowNumber - 1)(colNumber))
              }
              // below links
              if(rowNumber < (size - 2)) {
                node.below = Some(nodes(rowNumber + 1)(colNumber))

              }
              // right links
              if(colNumber < (size - 2)) {
                node.right = Some(nodes(rowNumber)(colNumber + 1))

              }
              // left links
              if(colNumber > 0) {
                node.left = Some(nodes(rowNumber)(colNumber - 1))

              }
          }

      }

    }

    def nodesAndStartPosFromMap(map: Map) : Node = {

      // we assume square
      val size = map.head.size

      val nodes: Vector[Vector[Node]] = map.map {
        row =>

          row.map {
            node =>

              if(node == '.') Node(Normal)
              else if (node == '#') Node(Infected)
              else throw new Exception(s"Invalid node type in map: $node")

          }

      }

      linkNodes(nodes)

      // return the middle node

      val middleRow = nodes(size / 2)
      middleRow(size / 2)
    }

    // Make a world from the string map

    def apply(s: String) : World = {

      // create an initial map which is just an array of char,
      // and we can use that to build a graph of Node
      val initialMap = mapFromString(s)

      val startNode = nodesAndStartPosFromMap(initialMap)

      World(startNode, Up, 0)
    }

  }

  case class World(currentNode: Node, direction: Direction, causedInfectionCount : Int) {

    // execute a burst of activity and produce a new world
    // note that this looks pure, but it's not, it mutates the Nodes in place

    // If the current node is infected, it turns to its right. Otherwise, it turns to its left. (Turning is done in-place; the
    // current node does not change.)
    // If the current node is clean, it becomes infected. Otherwise, it becomes cleaned. (This is done after the node is
    // considered for the purposes of changing direction.)
    // The virus carrier moves forward one node in the direction it is facing.

    def burst() : World = {

      val newDirection = if(currentNode.state == Infected) turnRight(direction) else turnLeft(direction)

      var newCount : Int = 0

      if(currentNode.state == Infected) {
        currentNode.state = Normal
        newCount = causedInfectionCount
      }
      else {
        currentNode.state = Infected
        newCount = causedInfectionCount + 1
      }

      val nextNode = newDirection match {

        case Up =>
          currentNode.above match {
            case Some(node) =>
              node
            case None =>
              Node
          }


      }

      World(nextNode, newDirection, newCount)

    }

  }

  def main(args: Array[String]): Unit = {

    val sampleMapStr = """..#
                         |#..
                         |...""".stripMargin

    // make the world from the map

    val sampleWorld = World(sampleMapStr)

    //printMap(sampleWorld.map)

    val step1MapStr = Resource.getAsString("input22.txt")(Charset.forName("US-ASCII"))

    val step1World = World(step1MapStr)

    var x = 1

    x = 2


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
 */