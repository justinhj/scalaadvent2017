
import Day11.Direction.Direction

import scala.annotation.tailrec
import scala.io.Source


object Day11 {

  object Direction extends Enumeration {
    type Direction = Value
    val N, NE, SE, S, SW, NW = Value

    val directions = List(N, NE, SE, S, SW, NW)

    def reverseDirection : Map[Direction,Direction] = Map (
      (NW, SE),
      (N, S),
      (NE, SW),
      (SE, NW),
      (S, N),
      (SW, NE)
    )

    def getAngle(direction: Direction) : Double = {
        direction.id * 60
    }

    def fromString(s: String) : Option[Direction] = {

      s match {
        case "nw" => Some(NW)
        case "n" => Some(N)
        case "ne" => Some(NE)
        case "se" => Some(SE)
        case "s" => Some(S)
        case "sw" => Some(SW)
        case _ => None
      }
    }
  }

  import HexNode._

  case class Connection(direction: Direction, from: HexNode, to: HexNode)

  case class HexGraph(root: HexNode, connection: Set[Connection])

  def buildGraph : HexGraph = {

    HexGraph(HexNode(getNextId, 0), Set.empty)

  }

  def findConnection(fromID: Int, direction: Direction, connections: Set[Connection]) : Option[Connection] = {

    connections.find {
      c =>
        fromID == c.from.id && direction == c.direction
    }

  }

  // expand each node on the graph until it reaches maxDistance
  def expandGraph(root: HexNode, maxDistance: Int, connections : Set[Connection], explored : Set[Int]) : (Set[Connection], Set[Int]) = {

    if(root.distance == maxDistance || explored.contains(root.id))
      (connections, explored)
    else {

      // for each direction from here either find or create a connection
      // keep track of new connections and explored nodes (to prevent backtracking)
      Direction.directions.foldLeft((connections, explored + root.id)) {
        case ((nc, newExplored), d) =>

          findConnection(root.id, d, connections) match {

            case Some(c) =>
              expandGraph(c.to, maxDistance, nc, newExplored)

            case None =>

              val newNode = HexNode(getNextId, root.distance + 1)
              val addNew = nc ++ Set(Connection(d, root, newNode), Connection(Direction.reverseDirection(d), newNode, root))

              expandGraph(newNode, maxDistance, addNew, newExplored)
          }

      }

    }

  }


  object HexNode {
    var nextID = 0

    def getNextId = {
      val id = nextID
      nextID = nextID + 1
      id
    }
  }

  // Represents a distance from the origin links to nodes in 6 directions
  // it has an ID so we can identify it when moving around
  case class HexNode(id: Int, distance: Int)

  // Expands the whole network by one step in every unexplored direction up to the max distance
//  def expandNetwork(origin: HexNode, maxDistance: Int) : HexNode = {
//
//    if(origin.distance == maxDistance) origin
//    else {
//      // Create a new node for each empty direction with the new distance
//
//      val updatedNodes = Direction.directions.map {
//        case d =>
//          origin.nodes.get(d) match {
//            case None =>
//
//              val reverseDirection = Direction.reverseDirection.get(d).get
//
//              (d, HexNode(origin.distance + 1, Map(reverseDirection -> origin)))
//
//            case Some(node) =>
//              (d, node)
//          }
//
//
//      }
//
//      origin.copy(nodes = updatedNodes.toMap)
//    }
//
//  }

  //case class ExploreState(origin: HexNode)

  // When taking a step we expand the whole network by one so we always have a big enough
  // network to cover the steps
  // Note that the network expands from the origin, not the current node since we may find a better
  // route than the random wanderer
  // Returns the new hexnode for taking the next step and the updated network origin
//  def move(exploreState: ExploreState, direction: Direction) : HexNode = {
//
//    val expanded = ExploreState.copy(origin = expandNetwork(exploreState.origin))
//
//    HexNode(0, nodes)
//
//  }

  def inputToDirections(input: String): List[Direction] = {
    input.split(',').toList.flatMap(Direction.fromString)
  }

  // Make substitutions to shorten the path

  def cancelDirections(dir1 : Direction, dir2 : Direction, directions : Map[Direction, Int]): Map[Direction, Int] = {

    val dir1Count = directions.getOrElse(dir1, 0)
    val dir2Count = directions.getOrElse(dir2, 0)

    val amountToCancel = Math.min(dir1Count, dir2Count)

    if(amountToCancel > 0) {

      directions.updated(dir1, dir1Count - amountToCancel).updated(dir2, dir2Count - amountToCancel)

    } else directions

  }

  // Convert pairs of dir1 and dir2 to the convertTo direction

  def convertPairs(dir1 : Direction, dir2 : Direction, convertTo: Direction, directions : Map[Direction, Int]): Map[Direction, Int] = {

    val dir1Count = directions.getOrElse(dir1, 0)
    val dir2Count = directions.getOrElse(dir2, 0)

    val amountToConvert = Math.min(dir1Count, dir2Count)

    if(amountToConvert > 0) {

      val convertToCount = directions.getOrElse(convertTo, 0)

      directions.
        updated(dir1, dir1Count - amountToConvert).
        updated(dir2, dir2Count - amountToConvert).
        updated(convertTo, convertToCount + amountToConvert)

    } else directions

  }

  // recursively shorten the path until it stops reducing

  import Direction._

  @tailrec
  def cancelUntilDone(directions: Map[Direction, Int]): Map[Direction, Int] = {

    val startLen = directions.values.sum

    val d1 = cancelDirections(N, S,directions)
    val d2 = cancelDirections(SW,NE,d1)
    val d3 = cancelDirections(SE,NW, d2)

    val d4 = convertPairs(NE, NW, N, d3)
    val d5 = convertPairs(SE, N, NE, d4)
    val d6 = convertPairs(NE, S, SE, d5)
    val d7 = convertPairs(SE, SW, S, d6)
    val d8 = convertPairs(S, NW, SW, d7)
    val d9 = convertPairs(N, SW, NW, d8)

    val finalLen = d9.values.sum

    if(startLen == finalLen) directions
    else cancelUntilDone(d9)

  }


  def sumDirections(steps: List[Direction]) : Int = {

    val nw = steps.filter(_ == Direction.NW).size
    val se = steps.filter(_ == Direction.SE).size

    val ne = steps.filter(_ == Direction.NE).size
    val sw = steps.filter(_ == Direction.SW).size

    val n = steps.filter(_ == Direction.N).size
    val s = steps.filter(_ == Direction.S).size

    val nwAbs = Math.abs(nw - se)
    val neAbs = Math.abs(ne - sw)

    // total distance back is abs(n - s)
    // can convert matching nw ne to a n and matching sw and se to a s to reduce distance

//    val matchingNWNE = Math.min(nw,ne)
//    val matchingSWSE = Math.min(sw,se)

    // replace pairs of NW and NE with S and pairs of SW and SE with N

    val nwnePairs = Math.min(nw, ne)
    val southsAfterReplace = s + nwnePairs
    val newnwAfterReplace = nw - nwnePairs
    val newneAfterReplace = ne - nwnePairs

    val swsePairs = Math.min(sw, se)
    val northsAfterReplace = n + swsePairs
    val newswAfterReplace = sw - swsePairs
    val newseAfterReplace = se - swsePairs

    // cancel out norths with se and sw
    val adjustedNorths = Math.max(0, northsAfterReplace  - (newswAfterReplace + newseAfterReplace))
    val adjustedSouths = Math.max(0, southsAfterReplace  - (newnwAfterReplace + newneAfterReplace))

    Math.abs(adjustedNorths - adjustedSouths) + Math.abs(newnwAfterReplace - newseAfterReplace) + Math.abs(newneAfterReplace - newswAfterReplace)
  }

  def ringCount(n: Int) = 6 * n

  val ringCounts = (1 to 20).foldLeft(List(0), 0) {
    case ((acc, total), n) =>
      val rc = ringCount(n)
      val newTotal = rc + total
      (newTotal :: acc, total + rc)
  }._1.reverse.zipWithIndex.map{case (a,b) => (b,a)}

  // Get the ring and hence the distance from origin based on an index n which is gotten by wrapping
  // hexes in rings from the centre outward and counting upwards as we go
  def ringFromNum(n : Int) : Int = {

    ringCounts.find{
      case (r, m) =>
        m >= n
    } match {
        case None =>
          0
        case Some(r) =>
          r._1
    }
  }

  object Vector2 {
    val origin = Vector2(0.0,0.0)

    // Checks the orientation of a movement vector to the origin and returns whether it is moving towards or away from
    // the origin and 0 for lateral movement
    // -1, 0 or 1

    def getOriginOrientation(pos: Vector2, moveVector: Vector2) : Double = {

      val toOrigin = pos.diff(Vector2.origin)
      toOrigin.dot(moveVector)

    }

    // Gets the direction vector to move a point in the given direction

    def moveDirection(dir: Direction) : Vector2 = {

      val n : Double = Direction.getAngle(dir).toRadians

      val newX = Math.sin(n)
      val newY = Math.cos(n)

      Vector2(newX, newY)
    }
  }

  case class Vector2(x: Double, y: Double) {

    def move(d: Vector2) : Vector2 = Vector2(d.x + x, d.y + y)

    def dot(a: Vector2) : Double = this.x * a.x + this.y + a.y

    def diff(a: Vector2): Vector2 = Vector2(this.x - a.x, this.y - a.y)

    def len : Double = {
      val sum = (x * x) + (y * y)

      Math.sqrt(sum)
    }

  }

  def followPath(directions: List[Direction]): Vector2 = {

    directions.foldLeft(Vector2.origin) {

      case (pos, dir) =>

        val moveVector = Vector2.moveDirection(dir)
        val newPos = pos.move(moveVector)

        println(s"Moved $dir from $pos to $newPos")

        newPos
    }

  }

  def getDistance(directions: List[Direction]): Int = {
    val finalPos = followPath(directions)

    val dist : Double = finalPos.len

    scala.math.round(dist).toInt

  }

  def getCancelledLength(directions: List[Direction]): Int = {

    val directionCount = directions.foldLeft(Map.empty[Direction, Int]) {

      case (acc, d) =>
        val count = acc.getOrElse(d, 0)
        acc updated (d, count + 1)
    }

    val cancelledCount = cancelUntilDone(directionCount)

    cancelledCount.values.sum
  }


  def main(args: Array[String]): Unit = {

    /*
      ne,ne,ne is 3 steps away.
      ne,ne,sw,sw is 0 steps away (back where you started).
      ne,ne,s,s is 2 steps away (se,se).
      se,sw,se,sw,sw is 3 steps away (s,s,sw).
     */

    val stepInput = Source.fromResource("input11.txt").mkString

    val sample1 = inputToDirections("ne,ne,ne")
    val sample2 = inputToDirections("ne,ne,sw,sw")
    val sample3 = inputToDirections("ne,ne,s,s")
    val sample4 = inputToDirections("se,sw,se,sw,sw")

    println(s"Sample1 ${getCancelledLength(sample1)}")
    println(s"Sample2 ${getCancelledLength(sample2)}")
    println(s"Sample3 ${getCancelledLength(sample3)}")
    println(s"Sample4 ${getCancelledLength(sample4)}")

    println(s"Step 1 ${getCancelledLength(inputToDirections(stepInput))}")

    // simple sum of ne and sw gives 3
//    assert(sumDirections(inputToDirections("ne,ne,ne")) == 3)
//
//    val r1 = followPath(inputToDirections("ne,ne,ne"))
//    println(s"$r1 len ${r1.len} d ${getDistance(inputToDirections("ne,ne,ne"))}")
//
//    // directions cancel out
//    assert(sumDirections(inputToDirections("ne,ne,sw,sw")) == 0)
//
//    val r2 = followPath(inputToDirections("ne,ne,sw,sw"))
//    println(s"$r2 len ${r2.len} d ${getDistance(inputToDirections("ne,ne,sw,sw"))}")
//
//    // you can cancel out a north or south with a se/sw or ne/nw respectively
//    assert(sumDirections(inputToDirections("ne,ne,s,s")) == 2)
//
//    val r3 = followPath(inputToDirections("ne,ne,s,s"))
//    println(s"$r3 len ${r3.len} d ${getDistance(inputToDirections("ne,ne,s,s"))}")

    // this seems to be the opposite
    // that pairs of SE and SW or NE and NW and be exchanged for a single N or S respectively

//    assert(sumDirections(inputToDirections("se,sw,se,sw,sw")) == 3)

//    val r4 = followPath(inputToDirections("se,sw,se,sw,sw"))
//    println(s"$r4 len ${r4.len} d ${getDistance(inputToDirections("se,sw,se,sw,sw"))}")

//    val step1 = sumDirections(inputToDirections(stepInput))

//    println(s"Step one answer : $step1")

//    val origin = HexNode.origin
//
//    val expanded = expandNetwork(origin, 1)


    //println(s"step one result = ${getDistance(inputToDirections(stepInput))}")

    //val rings = (0 to 21).map(n => ringFromNum(n))

    assert(ringFromNum(0) == 0)
    assert(ringFromNum(1) == 1)
    assert(ringFromNum(5) == 1)
    assert(ringFromNum(6) == 1)
    assert(ringFromNum(15) == 2)
    assert(ringFromNum(18) == 2)

//
//    val angles = Direction.directions.foreach {
//      d =>
//
//        println(s"d $d angle ${Direction.getAngle(d)}")
//
//        val startPos = Vector2(0.0, 1.0)
//
//        val moveVector = startPos.moveDirection(d)
//        val newPos = startPos.move(moveVector)
//
//        println(s"move vector $moveVector")
//        println(s"orientation ${Vector2.getOriginOrientation(startPos, moveVector)}")
//
//    }


    //val g = buildGraph
    val root = HexNode(getNextId, 0)
//    val e1 = expandGraph(root, 1, Set.empty, Set.empty)
//    val e2 = expandGraph(root, 2, e1._1, Set.empty)
//    val e3 = expandGraph(root, 3, e2._1, Set.empty)

    val expanded = (1 to 5).foldLeft(Set.empty[Connection]) {
      case (newConn, c) =>
        expandGraph(root, c, newConn, Set.empty)._1
    }

    def walkGraph(current: HexNode, dirs: List[Direction], connections: Set[Connection]) : HexNode = {

        dirs match {

          case d :: rest =>

            findConnection(current.id, d, connections) match {

              case Some(c) =>

                walkGraph(c.to, rest, connections)

              case None =>

                println(s"No connection from node $current")

                ???

            }

          case Nil =>

            current

        }


    }
//
//    val w1  = walkGraph(root, inputToDirections("ne,ne,ne"), expanded)
//    val w2 = walkGraph(root, inputToDirections("ne,ne,sw,sw"), expanded)
//    val w3 = walkGraph(root, inputToDirections("ne,ne,s,s"), expanded)
//    val w4 = walkGraph(root, inputToDirections("se,sw,se,sw,sw"), expanded)

    var x = 1

    x = 2


  }


}
