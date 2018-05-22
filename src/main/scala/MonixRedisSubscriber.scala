import com.redis.RedisClient

import scala.util.{Failure, Success, Try}

object MonixRedisSubscriber {


  def main(args: Array[String]): Unit = {

    Try(new RedisClient("127.0.0.1", 6379)) match {

      case Success(rc) =>

        println(s"Connected to Redis ${rc.host}:${rc.port}")

      case Failure(err) =>
        println(s"Failed to connect to Redis $err")

    }

  }

}
