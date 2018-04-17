package com.heyesjones.fs2redis

import cats.effect.{Effect, IO}
import com.redis._
import com.typesafe.scalalogging.LazyLogging
import fs2.{async, _}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object FS2RedisStream extends LazyLogging {

  case class RedisSubscriber(redis: RedisClient, channel: String) {

    // A user of this subscriber can set the callback
    var newMessageCallback : Option[Either[Throwable, PubSubMessage] => Unit] = _

    // Subscribe to the channel and pass any data through to the callback
    redis.subscribe(channel) {

      // We simply forward any M (messages) by way of the callback

      redisMessage => redisMessage match {
        case m @ M(_, message) if newMessageCallback isDefined =>
          newMessageCallback.get(Right(m))

        // If anything else happens we do nothing
        case _ =>

      }

    }

    def setCallback(cb: Either[Throwable, PubSubMessage] => Unit) = newMessageCallback = Some(cb)

  }

  // with returns unit. it takes a callback that takes a message (or error) and returns unit
  // in the sample code we call the function and set the callback to one that will enqueue (e)
  // the emitted message

  // Readers guide:
  // [1] We create an queue of the appropriate effect type as well as value type (our redis messages)
  //

  // https://underscore.io/blog/posts/2018/03/20/fs2.html
  // https://functional-streams-for-scala.github.io/fs2/guide.html#asynchronous-effects-callbacks-invoked-multiple-times

  def rows[F[_]](h : RedisSubscriber) (implicit F: Effect[F], ec: ExecutionContext): Stream[F,PubSubMessage] =
    for {
      q <- Stream.eval(async.unboundedQueue[F,Either[Throwable,PubSubMessage]]) // [1]
      _ <- Stream.eval { F.delay(h.setCallback(e => async.unsafeRunAsync(q.enqueue1(e))(_ => IO.unit)))} // [2]
      row <- q.dequeue.rethrow // [3]
    } yield row

  def main(args : Array[String]) : Unit = {

    Try(new RedisClient("127.0.0.1", 6379)) match {

      case Success(rc) =>

        logger.info(s"Connected to Redis ${rc.host}:${rc.port}")

        import scala.concurrent.ExecutionContext.Implicits.global

        val channelStream = rows[IO](RedisSubscriber(rc, "test1"))

        val done: List[PubSubMessage] = channelStream.take(5).compile.toList.unsafeRunSync()

        println(done)

      case Failure(exception) =>
        logger.error(s"Failed to connect to Redis. ${exception.getMessage}")
    }
  }
}