package com.heyesjones.fs2redis

import cats.effect.{Effect, IO}
import com.redis._
import com.typesafe.scalalogging.LazyLogging
import fs2._
import fs2.async
import scala.concurrent.ExecutionContext

import scala.util.{Failure, Success, Try}

object FS2RedisStream extends LazyLogging {

  case class RedisSubscriber(redis: RedisClient, channel: String) {

    // temp shitty converter from redis library callback to the
    // one that we want which handles errors
    def cbTranslate(pubSubMessage: PubSubMessage) : Either[Throwable, PubSubMessage] = {
      Right(pubSubMessage)
    }


    def withRows(cb: Either[Throwable, PubSubMessage] => Unit) = {

      redis.subscribe(channel){
        message => cbTranslate(message)
      }

    }

  }

  // Readers guide:
  // [1] We create an queue of the appropriate effect type as well as value type (our redis messages)
  //

  // https://underscore.io/blog/posts/2018/03/20/fs2.html
  // https://functional-streams-for-scala.github.io/fs2/guide.html#asynchronous-effects-callbacks-invoked-multiple-times

  def rows[F[_]](h : RedisSubscriber) (implicit F: Effect[F], ec: ExecutionContext): Stream[F,PubSubMessage] =
    for {
      q <- Stream.eval(async.unboundedQueue[F,Either[Throwable,PubSubMessage]]) // [1]

      _ <- Stream.eval { F.delay(h.withRows(e => async.unsafeRunAsync(q.enqueue1(e))(_ => IO.unit))) } // [2]

      row <- q.dequeue.rethrow // [3]
    } yield row

  //_ <-  Stream.eval { F.delay(redisclient.subscribe(channel, {e : PubSubMessage => async.unsafeRunAsync(q.enqueue1(e))})(_ => IO.unit)) } // [2]

//  val streamData: Stream[IO, Long] = Scheduler[IO](corePoolSize = 1).flatMap { scheduler =>
//    scheduler.awakeEvery[IO](10 milliseconds).map(f => f.toMillis)
//  }

  def main(args : Array[String]) : Unit = {


    Try(new RedisClient("127.0.0.1", 6379)) match {

      case Success(rc) =>

        logger.info(s"Connected to Redis ${rc.host}:${rc.port}")

        import scala.concurrent.ExecutionContext.Implicits.global

        val wut = rows[IO](RedisSubscriber(rc, "test1"))

        wut.take(5).compile.toList.unsafeRunSync()

      case Failure(exception) =>

        logger.error(s"Failed to connect to Redis. ${exception.getMessage}")

    }



  }

}
