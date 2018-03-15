
import cats.Eval
import cats.effect.IO
import fs2.{Chunk, Pure, Stream, text}
import cats.instances.all
import cats.implicits._

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

  // TODO better to use Array or Vector

  val a1 = Array[Int](1,2,3)



  def dance(input: String) = ???

//  Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For
//    example, s3 on abcde produces cdeab).
//    Exchange, written xA/B, makes the programs at positions A and B swap places.
//  Partner, written pA/B, makes the programs named A and B swap places.

  trait DanceMove

  case class Spin(len: Int) extends DanceMove {
    def fromString(s: String) : Option[DanceMove] = {

      val spinPattern = """s([0-9]+)""".r

      Try {
        val spinPattern(spinLen) = s

        Spin(spinLen.toInt)
      }.toOption

    }
  }

  case class Exchange(a: Int, b: Int) extends DanceMove {
    def fromString(s: String) : Option[DanceMove] = {

      val exchangePattern = """x([0-9]+)/([0-9]+)""".r

      Try {
        val exchangePattern(a,b) = s

        Exchange(a.toInt, b.toInt)
      }.toOption

    }
  }

  case class Partner(a: Char, b : Char) extends DanceMove {
    def fromString(s: String) : Option[DanceMove] = {

      val partnerPattern = """p([a-p])/([a-p])""".r

      Try {
        val partnerPattern(a,b) = s

        Partner(a(0), b(0))
      }.toOption

    }
  }

  object DanceMove {
    def parse(s: String) = {

      val tryParse = ()

    }
  }

  def main(args: Array[String]) : Unit = {

    val sample = "s1,x3/4,pe/b"

    // TODO FS2 Stream

    val input: Stream[Pure, String] = Stream.emit(sample)

    val splitOnCommas: Stream[Pure, String] = input.repartition{ s => Chunk.array(s.split(","))}

    val commands = splitOnCommas.map{DanceMove.fromString(_)}


    // create the three types of commands

    // read the input
    // delimit by comma
    // turn each string into a command
    // fold apply the commands to the start input




    println("day 16")

  }

}

/*
import cats.effect.{IO, Sync}
import fs2.{io, text}
import java.nio.file.Paths

def fahrenheitToCelsius(f: Double): Double =
  (f - 32.0) * (5.0/9.0)

def converter[F[_]](implicit F: Sync[F]): F[Unit] =
  io.file.readAll[F](Paths.get("testdata/fahrenheit.txt"), 4096)
    .through(text.utf8Decode)
    .through(text.lines)
    .filter(s => !s.trim.isEmpty && !s.startsWith("//"))
    .map(line => fahrenheitToCelsius(line.toDouble).toString)
    .intersperse("\n")
    .through(text.utf8Encode)
    .through(io.file.writeAll(Paths.get("testdata/celsius.txt")))
    .compile.drain

// at the end of the universe...
val u: Unit = converter[IO].unsafeRunSync()
*/