import cats.Eval
import cats.data.EitherT
import cats.effect.IO
import fs2.{Pipe, Pull, Pure, Scheduler, Stream}
import fs2.Scheduler._
import cats.effect.IO

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._

// Exercises and samples from
// https://functional-streams-for-scala.github.io/fs2/guide.html#resource-acquisition
// and other play

object FS2Play {

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

    def myDrain[F[_], A](stream: Stream[F,A]) : Stream[F,Nothing] = {

      Stream.empty

    }

    val s2 = Stream(1,2,3)
    val drained = myDrain(s2).toList

    println(s"drained $drained")

    case class MyStream[+F[_], +O](s: Stream[F,O]) {

      def myEval_[F[_]](fo: F[_]) : Stream[F,Nothing] = {

        val evalled: Stream[F, Any] = Stream.eval(fo)

        evalled.drain

      }

    }

    implicit def toMyStream[F[_],O](s: Stream[F,O]) : MyStream[F,O] = ???

    def myEval_[F[_]](fo: F[_]) : Stream[F,Nothing] = {

      val evalled: Stream[F, Any] = Stream.eval(fo)

      evalled.drain

    }

    def futureHello(whenInTheFuture: FiniteDuration) : Future[String] ={

      Future {
        Thread.sleep(whenInTheFuture.toMillis)
        s"String from $whenInTheFuture in the past"
      }
    }


    // Takewhile condition is true
    def myTakewhile[F[_],O](fo : (O => Boolean)): Pipe[F,O,O] = {

      def go(s: Stream[F,O], fo : (O => Boolean)): Pull[F,O,Unit] = {
        s.pull.uncons.flatMap {
          case Some((hd,tl)) =>

            Pull.segment(hd.takeWhile(fo)).flatMap {
              case Left((a,rem)) =>
                go(tl,fo)
              case Right(a) =>
                Pull.done
            }
          case None =>
            Pull.done
        }
      }

      in =>
        go(in,fo).stream
    }

    println(
      Stream.range(0,100).through(myTakewhile{a : Int => a < 7}).toList)


    val s3: IO[Unit] = IO(println("s3 output !!"))
    val eval: Stream[IO, Nothing] = myEval_(s3)

    val evalled = eval.compile.toVector
    val runned = evalled.unsafeRunSync()

    println(s"my eval $runned")

    // stream of future?

    val f1 : Future[String] = futureHello(2 seconds)

    val ef1: Stream[Future, Nothing] = myEval_(f1)

    val ef1c = ef1.compile

    // cannot toVector or Run it?


    // how about list

    val l1 = List(1,2,3)

    val el1e = myEval_(l1)

    // needs an implicit of some sort
    //el1e.compile.toVector


    // attempt
    //(Stream(1,2) ++ (throw new Exception("nooo!!!"))).attempt.toList
    // res30: List[Either[Throwable,Int]] = List(Right(1), Right(2), Left(java.lang.Exception: nooo!!!))

    val errorStream = Stream(1,2) ++ (throw new Exception("nooo!!!"))

    // periodically emit something

//    val s4 = Scheduler(corePoolSize = 1)

    //implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global
    val streamData: Stream[IO, String] = Scheduler[IO](corePoolSize = 2).flatMap {
      scheduler =>
        scheduler.awakeEvery[IO](1.second).map{
          _ =>
            (System.currentTimeMillis() % 10000).toString}
    }


//    val hmm = streamData.compile.toVector
//
//    val out = hmm.unsafeRunSync()
//
//    // print the time
//
//    val ios = streamData.map {
//
//      time =>
//        IO( println(s"the time is $time") )
//
//    }
//
//    ios.compile.drain
//
//    Thread.sleep ( 10000 )

    //periodicHello.


  }

}
