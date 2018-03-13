import cats.Eval
import cats.effect.IO
import fs2.{Pure, Stream}
import cats.instances.all

object Day13FS2 {

  val testInput = """0: 3
                    |1: 2
                    |4: 4
                    |6: 4""".stripMargin

  val testInputLines = testInput.lines.toList

  def parseLine(line: String) : (Int,Int) = {

    val pattern = """([-]*\d+): ([-]*\d+)""".r

    val pattern(layer, range) = line

    (layer.toInt, range.toInt)
  }

  def parseInput(lines: List[String]) : Map[Int,Int] = {

    lines.map(parseLine).toMap
  }

  // The number of layers is equal to the largest layer in the data set

  def getNumLayers(layers: Map[Int,Int]) = layers.keys.max

  // scanners go up and down so we cannot simply mod with time to get the position
  // but we can get the period of movement with this formula ...

  def getPeriod(range: Int) =
    Math.max(1, range + (range - 2))

  def scannerAtZero(time: Int, range: Int) : Boolean = {

    val period = getPeriod(range)

    time % period == 0

  }

  // Simulate traversal

  def simulate(layers: Map[Int,Int]) : Int = {

    val numLayers = getNumLayers(layers)

    val total = (0 to numLayers).foldLeft(0) {
      case (caughtCost, time) =>

        // get layer if any. note that time and position
        // are the same since we move at one step per picosecond

        layers.get(time) match {
          case None =>
            //println(s"No layer at $time")
            caughtCost
          case Some(range) =>

            val caught = scannerAtZero(time, range)

            //println(s"range is $range, caught $caught at time $time")

            if(caught) caughtCost + (time * range)
            else caughtCost
        }
    }

    total

  }

  // part two

  // To use the same solution for part 2 we need to allow the position we are at to be at an offset to the
  // real time, so we can adjust simulate to do that

  def simulateWithOffset(layers: Map[Int,Int], delay: Int) : Int = {

    val numLayers = getNumLayers(layers)

    val total = (0 to numLayers).foldLeft(0) {
      case (caughtCost, time) =>

        // get layer if any. note that time and position
        // are the same since we move at one step per picosecond

        layers.get(time) match {
          case None =>
            //println(s"No layer at $time")
            caughtCost
          case Some(range) =>

            // adjust time

            val offsetTime = time + delay

            val caught = scannerAtZero(offsetTime, range)

            //println(s"range is $range, caught $caught at time $time offset time $offsetTime")

            if(caught) {

              // This cost me a couple of hours hammock time. In step 2 we want to count a cost of zero
              // as still being caught, so always return at least one ...

              Math.max(1, caughtCost + (time * range))
            }
            else caughtCost
        }
    }

    total

  }

  // Use FS2 streams
  // Demonstrates parallel and short circuiting

  def getEvadeFirewallDelay(layers: Map[Int,Int]) : Int = {



    ???
  }

  // delay longer and longer until we succeed
  def getEvadeFirewallDelay(layers: Map[Int,Int], delay : Int = 0) : Int = {

    if(delay % 100000 == 0) println(s"Sim delay $delay")

    val simCost = simulateWithOffset(layers, delay)

    if(simCost == 0)
      delay
    else
      getEvadeFirewallDelay(layers, delay + 1)
  }


  def main(args: Array[String]) : Unit = {


    // delay is an increasing value from 0 until we find a solution

    val delayStream: Stream[Pure, Int] = Stream.range(0, 20)

    //val test10: Stream[Eval, Int] = delayStream.take(10) // see range

    val test10_2: Stream[Pure, Int] = Stream.range(0, 10)

    val output: Stream[Pure, Int] = delayStream ++ test10_2

    val what = output.compile
    //val whatNow: Pure[List[Int]] = what.toList

    //println("list " + output.compile.toList)


    val io1: Stream[IO, Unit] = Stream.eval(IO{ println("IO happened yay") })

    io1.compile.drain.unsafeRunSync()

    io1.compile.drain.unsafeRunSync()

    def myRepeat[F[_], A](stream: Stream[F,A]) : Stream[F,A] = {
      stream ++ myRepeat(stream)
    }

    val s : Stream[Pure, Int] = Stream(1,0)

    val repeated = myRepeat(s).take(6).toList

    println(s"repeated $repeated")

    def myDrain[F[_], A](stream: Stream[F,A]) : Stream[F,A] = {

      Stream.empty

    }

    val s2 = Stream(1,2,3)
    val drained = myDrain(s2).toList

    println(s"drained $drained")

    // Step 1 is simple, we just recursively determine group 0 and output the size

//    val step1Input = parseInput(step1InputLines)
//    val testInput = parseInput(testInputLines)
//
//    assert(simulate(testInput) == 24)
//
//    println(s"Step 1: ${simulate(step1Input)}")
//
//    // Step 2
//
//    assert(getEvadeFirewallDelay(testInput) == 10)
//
//    println(s"Step 2: ${getEvadeFirewallDelay(step1Input)}")


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

