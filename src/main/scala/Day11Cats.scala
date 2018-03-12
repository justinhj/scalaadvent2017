import Day11Cats.Direction.Direction
import cats.data.State

import scala.io.Source

object Day11Cats {

  object Direction extends Enumeration {
    type Direction = Value
    val N, NE, SE, S, SW, NW = Value

    val directions = List(N, NE, SE, S, SW, NW)

    // Try to convert a string to a direction
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

  // Convert input string to a list of directions

  def inputToDirections(input: String): List[Direction] = {
    input.split(',').toList.flatMap(Direction.fromString)
  }

  type DirectionCountMap = Map[Direction, Int]

  type DirectionsState[A] = State[DirectionCountMap, A]

  val step1Input = Source.fromResource("input11.txt").mkString
  val step1Directions = inputToDirections(step1Input)

  def getDirectionCount(directions : List[Direction]) = directions.foldLeft(Map.empty[Direction, Int]) {

    case (acc, d) =>
      val count = acc.getOrElse(d, 0)
      acc updated (d, count + 1)
  }

  def getCount : DirectionsState[Int] = State {
    s =>
      (s, s.values.sum)
  }

  // Cancel out dir1 and dir2 to shorten the path

  def cancelDirections(dir1 : Direction, dir2 : Direction) : DirectionsState[Unit] = State {

    directions =>

      val dir1Count = directions.getOrElse(dir1, 0)
      val dir2Count = directions.getOrElse(dir2, 0)

      val amountToCancel = Math.min(dir1Count, dir2Count)

      if(amountToCancel > 0) {

        (directions.updated(dir1, dir1Count - amountToCancel).updated(dir2, dir2Count - amountToCancel), ())

      } else (directions, ())

  }

  // Convert pairs of dir1 and dir2 to the convertTo direction

  def convertPairs(dir1 : Direction, dir2 : Direction, convertTo: Direction) : DirectionsState[Unit] = State {

    directions =>
      val dir1Count = directions.getOrElse(dir1, 0)
      val dir2Count = directions.getOrElse(dir2, 0)

      val amountToConvert = Math.min(dir1Count, dir2Count)

      if(amountToConvert > 0) {

        val convertToCount = directions.getOrElse(convertTo, 0)

        (directions.
          updated(dir1, dir1Count - amountToConvert).
          updated(dir2, dir2Count - amountToConvert).
          updated(convertTo, convertToCount + amountToConvert), ())

      } else (directions, ())

  }

  import Day11Cats.Direction._

  def getCancelledLength(directions : List[Direction]) : Int = {

    val dirCount = getDirectionCount(directions)

    val cancel = for (
      _ <- cancelDirections(N, S);
      _ <- cancelDirections(SW,NE);
      _ <- cancelDirections(SE,NW);

      _ <- convertPairs(NE, NW, N);
      _ <- convertPairs(SE, N, NE);
      _ <- convertPairs(NE, S, SE);
      _ <- convertPairs(SE, SW, S);
      _ <- convertPairs(S, NW, SW);
      _ <- convertPairs(N, SW, NW);

      count <- getCount

    ) yield count

    cancel.runA(dirCount).value
  }


  def main(args: Array[String]) : Unit = {

    val sample1 = inputToDirections("ne,ne,ne")
    val sample2 = inputToDirections("ne,ne,sw,sw")
    val sample3 = inputToDirections("ne,ne,s,s")
    val sample4 = inputToDirections("se,sw,se,sw,sw")

    assert(getCancelledLength(sample1) == 3)
    assert(getCancelledLength(sample2) == 0)
    assert(getCancelledLength(sample3) == 2)
    assert(getCancelledLength(sample4) == 3)

    val s1 = getCancelledLength(step1Directions)

    println(s"Step 1 : $s1")

    val steps = inputToDirections(step1Input)

    val furthest = (1 to steps.size).foldLeft(0) {

      case (max, numSteps) =>

        val thisSteps = steps.take(numSteps)

        val dist = getCancelledLength(thisSteps)

        if(dist > max) dist
        else max
    }

    println(s"Step 2 : $furthest")


  }



}
