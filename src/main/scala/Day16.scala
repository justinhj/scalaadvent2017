
import cats.Eval
import cats.effect.IO
import fs2.{Chunk, Pure, Stream, text}
import cats.instances.all
import cats.implicits._
import com.sun.org.omg.CORBA.ExcDescriptionSeqHelper

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

object Day16 {

//  Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For
//    example, s3 on abcde produces cdeab).
//    Exchange, written xA/B, makes the programs at positions A and B swap places.
//  Partner, written pA/B, makes the programs named A and B swap places.

  trait DanceMove

  case class Spin(len: Int) extends DanceMove

  object Spin {
    def fromString(s: String) : Option[DanceMove] = {

      val spinPattern = """s([0-9]+)""".r

      Try {
        val spinPattern(spinLen) = s

        Spin(spinLen.toInt)
      }.toOption

    }
  }

  case class Exchange(a: Int, b: Int) extends DanceMove

  object Exchange {
    def fromString(s: String) : Option[DanceMove] = {

      val exchangePattern = """x([0-9]+)/([0-9]+)""".r

      Try {
        val exchangePattern(a,b) = s

        Exchange(a.toInt, b.toInt)
      }.toOption

    }
  }

  case class Partner(a: Char, b : Char) extends DanceMove

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

    // Execute a move
    def execute(dm : DanceMove, dancers: Vector[Char]) : Vector[Char] = {

      dm match {
        // Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For
        // example, s3 on abcde produces cdeab).
        case Spin(len) =>
          val spinCount = dancers.size - len
          dancers.drop(spinCount) ++ dancers.take(spinCount)

        // Exchange, written xA/B, makes the programs at positions A and B swap places.
        case Exchange(a,b) =>
          val elementAtA = dancers(a)
          val elementAtB = dancers(b)
          dancers.updated(a, elementAtB).updated(b, elementAtA)

        //Partner, written pA/B, makes the programs named A and B swap places.
        case Partner(a,b) =>
          val positionOfA = dancers.indexOf(a)
          val positionOfB = dancers.indexOf(b)
          execute(Exchange(positionOfA, positionOfB), dancers)

      }

    }

    def executeDance(startPositions : Vector[Char], moves: String) : Vector[Char] = {

      // read the input
      // delimit by comma
      // turn each string into a command
      // apply the commands to the start input

      val input: Stream[Pure, String] = Stream.emit(moves)

      val splitOnCommas: Stream[Pure, String] = input.repartition{ s => Chunk.array(s.split(","))}

      val commands: Stream[Pure, DanceMove] = splitOnCommas.map{in => DanceMove.fromString(in)}

      val temp = commands.toList

      val finalPositions: Stream[Pure, Vector[Char]] = commands.fold(startPositions) {
        case (dancers, move) =>
          DanceMove.execute(move, dancers)
      }

      finalPositions.toList.head

    }

  }

  def main(args: Array[String]) : Unit = {

    val sample = "s1,x3/4,pe/b"

    val danceResult = DanceMove.executeDance(Vector('a','b','c','d','e'), sample)
    assert(danceResult == Vector('b','a','e','d','c'))

    // test moves individually

    val spinTest = DanceMove.execute(Spin(1), Vector('a','b','c','d','e'))
    assert(spinTest == Vector('e','a','b','c','d'))

    val exchangeTest = DanceMove.execute(Exchange(3,4), Vector('e','a','b','c','d'))
    assert(exchangeTest == Vector('e','a','b','d','c'))

    val partnerTest = DanceMove.execute(Partner('e','b'), Vector('e','a','b','d','c'))
    assert(partnerTest == Vector('b','a','e','d','c'))

    val step1Dancers = Vector('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p')

    val step1Input = Source.fromResource("input16.txt").mkString

    val step1 = DanceMove.executeDance(step1Dancers, step1Input)

    println(s"day 16 answer for step 1 ${step1.mkString}")

    // step 2 is to do this 1 billion times

    val step2 = (1 to 1000000000).foldLeft(step1Dancers) {

      case (dancers, n) =>
        if(n % 100 == 0) println(s"n = $n")
        DanceMove.executeDance(dancers, step1Input)
    }

    println(s"day 16 answer for step 2 ${step2.mkString}")

  }

}
