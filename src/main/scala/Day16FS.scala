
import cats.implicits._
import fs2.{Chunk, Pure, Stream}

import scala.language.higherKinds

import scala.io.Source
import scala.util.Try

/**
  * --- Day 16: Permutation Promenade ---

You come upon a very unusual sight; a group of programs here appear to be dancing.

There are sixteen programs in total, named a through p. They start by standing in a line: a stands in position 0, b
stands in position 1, and so on until p, which stands in position 15.

The programs' dance consists of a sequence of dance moves:

    Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For
example, s3 on abcde produces cdeab).
    Exchange, written xA/B, makes the programs at positions A and B swap places.
    Partner, written pA/B, makes the programs named A and B swap places.

For example, with only five programs standing in a line (abcde), they could do the following dance:

    s1, a spin of size 1: eabcd.
    x3/4, swapping the last two programs: eabdc.
    pe/b, swapping programs e and b: baedc.

After finishing their dance, the programs end up in order baedc.

You watch the dance for a while and record their dance moves (your puzzle input). In what order are the programs
standing after their dance?

  */

object Day16FS {

//  Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For
//    example, s3 on abcde produces cdeab).
//    Exchange, written xA/B, makes the programs at positions A and B swap places.
//  Partner, written pA/B, makes the programs named A and B swap places.

  trait DanceMove {
    def execute(dancers: Vector[Char]) : Vector[Char]
  }

  case class Spin(len: Int) extends DanceMove {

    def execute(dancers: Vector[Char]) : Vector[Char] = {
      val spinCount = dancers.size - len
      dancers.drop(spinCount) ++ dancers.take(spinCount)
    }

  }

  object Spin {
    def fromString(s: String) : Option[DanceMove] = {

      val spinPattern = """s([0-9]+)""".r

      Try {
        val spinPattern(spinLen) = s

        Spin(spinLen.toInt)
      }.toOption

    }
  }

  case class Exchange(a: Int, b: Int) extends DanceMove {
    def execute(dancers: Vector[Char]) : Vector[Char] = {
      val elementAtA = dancers(a)
      val elementAtB = dancers(b)
      dancers.updated(a, elementAtB).updated(b, elementAtA)
    }
  }

  object Exchange {
    def fromString(s: String) : Option[DanceMove] = {

      val exchangePattern = """x([0-9]+)/([0-9]+)""".r

      Try {
        val exchangePattern(a,b) = s

        Exchange(a.toInt, b.toInt)
      }.toOption

    }
  }

  case class Partner(a: Char, b : Char) extends DanceMove {
    def execute(dancers: Vector[Char]) : Vector[Char] = {
      val positionOfA = dancers.indexOf(a)
      val positionOfB = dancers.indexOf(b)
      Exchange(positionOfA, positionOfB).execute(dancers)
    }
  }

  object Partner {
    def fromString(s: String) : Option[DanceMove] = {

      val partnerPattern = """p([a-p])/([a-p])""".r

      Try {
        val partnerPattern(a,b) = s

        Partner(a(0), b(0))
      }.toOption

    }
  }

  object DanceMove {

    // Figure out which dance move by the first letter then use the appropriate parser
    // Note we will throw an exception here if the DanceMove cannot be parsed
    // through the use of .get on the option
    // We will also throw an exception if the first letter is not a valid dance move
    // In a real system you'd want to handle the errors better, for example use
    // an Either and return the reason the parse failed to the caller

    def fromString(s: String) : DanceMove = {

      s(0) match {
        case 'p' => Partner.fromString(s).get
        case 's' => Spin.fromString(s).get
        case 'x' => Exchange.fromString(s).get
      }

    }

    def inputMovesToCommands(moves: String): Stream[Pure, DanceMove] = {

      val input: Stream[Pure, String] = Stream.emit(moves)

      val splitOnCommas: Stream[Pure, String] = input.repartition{ s => Chunk.array(s.split(","))}

      val commands: Stream[Pure, DanceMove] = splitOnCommas.map{in => DanceMove.fromString(in)}

      commands
    }

    def executeDance(startPositions : Vector[Char], moves: String) : Vector[Char] = {

      // read the input
      // delimit by comma
      // turn each string into a command
      // apply the commands to the start input
      // repeat with output dancers repeat times

      val commands = inputMovesToCommands(moves)

      val finalPositions = commands.fold(startPositions) {
        case (dancers, move) =>
          move.execute(dancers)
      }

      finalPositions.toList.head

    }

  }

  def main(args: Array[String]) : Unit = {

    val sample = "s1,x3/4,pe/b"

    val danceResult = DanceMove.executeDance(Vector('a','b','c','d','e'), sample)
    assert(danceResult == Vector('b','a','e','d','c'))

    // test moves individually

    val spinTest = Spin(1).execute(Vector('a','b','c','d','e'))
    assert(spinTest == Vector('e','a','b','c','d'))

    val exchangeTest = Exchange(3,4).execute(Vector('e','a','b','c','d'))
    assert(exchangeTest == Vector('e','a','b','d','c'))

    val partnerTest = Partner('e','b').execute(Vector('e','a','b','d','c'))
    assert(partnerTest == Vector('b','a','e','d','c'))

    val step1Dancers = Vector('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p')

    val step1Input = Source.fromResource("input16.txt").mkString

    val step1 = DanceMove.executeDance(step1Dancers, step1Input)

    println(s"day 16 answer for step 1 ${step1.mkString}")

    // step 2 is to do this 1 billion times

    // It takes about 1 second to do 100 so it will take 115 days to complete!

    // So what if it loops periodically? I'm not sure if any input of rules is guaranteed to loop since there
    // are 16! different dance positions (20,922,789,888,000)
    // If it was possible to use the dance moves to generate all the combinations it could easily exceed
    // 1b, (not sure if it is or not)
    // Anyway let's just empirically find a loop ...
    
    val step2 = (1 to 40).foldLeft(step1Dancers) {

      case (dancers, n) =>

        val nextStep = DanceMove.executeDance(dancers, step1Input)

        if(n % 100 == 0) println(s"n = $n")

        if(nextStep == step1Dancers) println(s"start repeated at $n")

        nextStep

    }

    // the nearest multiple of 60 to 1b is 999,999,960

    // so we can start there and run 40 more times to get the answer

    /// now we can simply start at the multiple before 1 billion

    // knmdfoijcbpghlea

    println(s"day 16 answer for step 2 ${step2.mkString}")

  }

}

/*
What follows is a flawed solution but I've left it here like a fossil of thought...

I assumed I could speed the dance up by calculating the delta each dancer went through between each dance
so we could run through the dance program much faster.

Why is this flaws? Well each dancer will go through a different path because of the partner step that
swaps elements by name not position.

This technique did reduce the runtime from 115 days to 15 minutes, but sadly is not the correct answer.


 */

// What is the correct position of a character? Its ascii value - the ascii value of 'a'
//    def getPosition(c: Char) : Int = {
//      val aVal = 'a'.toInt
//
//      c.toInt - aVal
//    }

// Find the original index of this character
//    def getOriginIndex(start: Vector[Char], c: Char) : Int = {
//      start.indexOf(c)
//    }

// This is flawed, doesn't take into account partner swaps

//    def generateDeltas(start: Vector[Char], end: Vector[Char]) : Vector[Int] = {
//
//      end.zipWithIndex.map {
//
//        case (c, index) =>
//
//          val whatShouldBeHere = start(index)
//
//          val whereItIs = end.indexOf(whatShouldBeHere)
//
//          whereItIs - index
//      }
//    }

//val exchanges = generateDeltas(step1Dancers, step1Final)

// rebuild the dance with the final exchanges

//    def fastDance(start: Vector[Char], exchanges: Vector[Int]) : Vector[Char] = {
//
//      start.zipWithIndex.foldLeft(start) {
//        case (acc, (c, index)) =>
//
//          acc.updated(index + exchanges(index), c)
//      }
//
//
//    }
//
//
//    val testFastDance = fastDance(step1Dancers, exchanges)
//
//    val step2: Vector[Char] = (1 to 1000000000).foldLeft(step1Dancers) {
//
//      case (dancers, n) =>
//        if(n % 1000000 == 0) println(s"n = $n")
//        fastDance(dancers, exchanges)
//    }















