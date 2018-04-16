
import cats.effect.IO
import fs2.{Pipe, Pull, Pure, Scheduler, Segment, Stream}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.annotation.tailrec

// Exercises and samples from
// https://functional-streams-for-scala.github.io/fs2/guide.html#resource-acquisition
// and other play

object FS2Play {

  // Sort a list of numbers using a stream of IO
  // Note that it will take n seconds to run, where n is the largest integer in your list
  def sleepSort(n : List[Int]): List[Int] = {

    val wut: Stream[IO, List[Int]] = Stream.emits(n).covary[IO].flatMap{ t => Stream.eval(IO{ Thread.sleep(t * 1000L); println(s"$n"); n })}

    val wut2: Stream.ToEffect[IO, List[Int]] = wut.compile

//    val s1 = Scheduler[IO](4)
//
//    val s2 = s1.flatMap {
//      scheduler =>
//        scheduler.awakeEvery(4 seconds)
//    }
//
//    s2.merge(wut)

    ???

  }

  def put(n: Int): IO[Unit] = IO(println(s"s is $n"))

  def printRange(v: Int) : Stream[IO, Unit] = {
    Stream.range(1, v).evalMap[IO, Unit]{n => put(n)}
  }

  //def seconds = Scheduler(1)


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

    def tk[F[_],O](n: Long): Pipe[F,O,O] = {
      def go(s: Stream[F,O], n: Long): Pull[F,O,Unit] = {
        s.pull.uncons.flatMap {
          case Some((hd: Segment[O, Unit],tl : Stream[F,O])) =>

            Pull.segment(hd.take(n)).flatMap {
              case Left((_,rem)) =>
                go(tl,rem)
              case Right(em: Segment[O, Unit]) =>

                //output1(em.)
                Pull.done
            }
          case None => Pull.done
        }
      }
      in => go(in,n).stream
    }

    def takeWhileRepeat[F[_],O](n: Long, f: O => Boolean): Pipe[F,O,O] = {

      def go(s: Stream[F,O], wasTrueCount : Int) : Pull[F,O,Unit] = {

        if(wasTrueCount == n) {
          Pull.done
        }
        else {
          s.pull.uncons1.flatMap {

            case Some((o, tl)) =>

              if(o == wasTrueCount) {
                Pull.output1(o) >> go(tl, wasTrueCount + 1)
              }
              else {
                Pull.output1(o) >> go(tl, wasTrueCount)
              }

            case None =>
              Pull.done

          }
        }
      }

      in => go(in, 0).stream

    }

    //scan[O2](z: O2)(f: (O2, O) => O2): Stream[F, O2]

    def scan[F[_],O](z : O, f : (O,O) => O): Pipe[F,O,O] = {

      def go(s: Stream[F,O], acc : O) : Pull[F,O,Unit] = {


          s.pull.uncons1.flatMap {

            case Some((o, tl)) =>

              val nextVal = f(acc, o)

              Pull.output1(nextVal) >> go(tl, nextVal)

            case None =>
              Pull.done

          }

      }

      in => go(in, z).stream

    }

    // Takewhile condition is true
    def myTakewhile[F[_],O](fo : (O => Boolean)): Pipe[F,O,O] = {

      def go(s: Stream[F,O], fo : (O => Boolean)): Pull[F,O,Unit] = {
        s.pull.uncons.flatMap {
          case Some((hd,tl)) =>

            Pull.segment(hd.takeWhile(fo)).flatMap {
              case Left(_) =>
                go(s, fo)
              case Right(seg) =>

                Pull.done // output(seg)
            }

          case None =>
            Pull.done
        }
      }

      in =>
        go(in,fo).stream
    }

    // Intersperse

    def intersperse[F[_],O](io: O): Pipe[F,O,O] = {

      def go(s: Stream[F,O]) : Pull[F,O,Unit] = {

          s.pull.uncons1.flatMap {

            case Some((o, tl)) =>
              Pull.output1(o) >> Pull.output1(io) >> go(tl)

            case None =>
              Pull.done

          }

      }

      in => go(in).stream

    }

    // take n using scanSegmentsOpt
    def tksso[F[_],O](n: Long): Pipe[F,O,O] = {
      in =>
        in.scanSegmentsOpt(n) { n =>
          if (n <= 0) None
          else Some(seg => seg.take(n).mapResult {
            case Left((_, n)) =>
              n
            case Right(s) =>
              0
          })
        }
    }

    val cs = Stream.emits("absbsbsbaaabssbbcccaaa")

    println("take until seen 5 'a's ",
      cs.through(takeWhileRepeat(5, {o => o == 'a'})).toList)

    println("zip with previous and next . ",
      Stream.range(1, 10).zipWithPreviousAndNext.toList)


    println("take 5 . ",
      Stream.range(1, 100).through(tksso(5)).toList)


    println("take until 7 numbers divisible by 7 . ",
      Stream.range(1, 100).through(takeWhileRepeat(7, {n => n % 7 == 0})).toList)


    println("intersperse with 8's . ",
      Stream.range(1, 10).through(intersperse(8)).toList)

    println("less than 7! ",
      Stream.range(0,100).through(myTakewhile{a : Int => a < 7}).toList)

    val res = Stream.range(0,10).through(scan(0, {(acc : Int, n: Int) => acc + n})).toList

    println("running sum", res)

    Stream.range(0,100).takeWhile(_ < 7).toList
    //Stream.repeat(10).through(takeWhile{_ < 2})

    println("less than 7 from empty stream ",
      Stream.empty.through(myTakewhile[Pure, Int]{a : Int => a < 7}).toList)

    println("less than 7 from empty stream ",
      Stream.eval_{a : Int => a < 7})

    val s3 = Stream.eval(IO(println("s3 output !!")))
    val s4 = Stream.eval(IO({println("s4 output!"); 3}))

    val s5: Stream[IO, AnyVal] = s3 ++ s4

    println(s"s5 ${s5.compile.drain.unsafeRunSync()}")

    //val eval: Stream[IO, Nothing] = myEval_(s3)

//    val evalled = eval.compile.toVector
//    val runned = evalled.unsafeRunSync()
//
//    println(s"my eval $runned")

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

    // This requires an execution context in scope
    // eval means execute this IO for its effects and return the stream
    // stream is in types O2 and O where O2 is >: to O
    // which means???

//    val ass = Stream(1,2,3).
//      merge(Stream.eval(IO { Thread.sleep(400); 0 })).
//      merge(Stream.eval(IO { Thread.sleep(200); 7 }))
//      .joinUnbounded
//
//    println("concurrent merge")


   // case class Message(response: String)

//    def healthCheck: Stream[IO, Message] = ???
//    def kafkaMessages: Stream[IO, Message] = ???
//    def celsiusConverter: Stream[IO, Unit] = ???
//
//    def thing: Stream[Pure, Stream[IO, Message]] = Stream(
//      healthCheck,
//      celsiusConverter.drain,
//      kafkaMessages
//    )
//
//    def all: Stream[IO, Message] = thing.covary[IO].joinUnbounded
//
//    def hello = all.flatMap{
//      f =>
//
//        Stream(f.response)
//    }

    /*

  /// how to make sure a stream only runs once using a signal

            Stream.eval(fs2.async.signalOf[IO, Boolean](false)).flatMap { interruptSignal =>
            server[IO](new InetSocketAddress("localhost", TCPPort))
                .interruptWhen(interruptSignal)
                .flatMap(stream => stream.flatMap(socket => sendStreamInTCP[IO](socket, dataStream)) ++ Stream.eval_(interruptSignal.set(true)))
        }.compile.drain.unsafeRunSync()


     */

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


    //sleepSort((1 to 10).toList)




    printRange(20).compile.drain.unsafeRunSync()


  }

}
