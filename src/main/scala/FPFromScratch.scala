import scala.annotation.tailrec
import scala.language.{higherKinds, implicitConversions}

object FPFromScratch {

  // Implement abstract functor and monad

  trait Functor[F[_]] {
    def myMap[A,B](fa : F[A], fab : A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {

        def myPure[A](a: A): F[A]

        def myFlatMap[A,B](v : F[A], f : A => F[B]) : F[B]

        // A Monad can implement map in terms of pure and flatmap

        def myMap[A,B](fa: F[A], fab : A => B): F[B] = {
          myFlatMap[A,B](fa, a => myPure(fab(a)))
        }
      
    }

    // Implement for Option

    class OptionMonad extends Monad[Option] {

      def myPure[A](a: A) = Some(a)

      def myFlatMap[A, B](fa: Option[A], f: A => Option[B]): Option[B] = {

        fa match {
          case Some(a) => f(a)
          case None => None
        }

      }

    }

    // Implicit conversion from Option to Monad extender

    case class OptionMonadExtender[A](private val o : Option[A]) extends OptionMonad {

      // need to add this function to allow single argument
      implicit def myMap[B](f: A => B) : Option[B] = myMap(o, f)
    }

    implicit def optionMonad[A](o : Option[A]) : OptionMonadExtender[A] = OptionMonadExtender(o)

  // Implement for list

    class ListMonad extends Monad[List] {

      def myPure[A](a: A) = List(a)

      // recursive implementation of flatmap that accumulates a list
      // of the results as we go...
      def myFlatMap[A, B](fa: List[A], f: A => List[B]): List[B] = {

        def fm(fa: List[A], f: A => List[B]) : List[B] = {

          fa match {
            case hd :: tl =>
              val m = f(hd)
              m ++ fm(tl, f)

            case Nil =>
              Nil
          }

        }

        fm(fa, f)
      }

    }

  case class ListMonadExtender[A](l : List[A]) extends ListMonad

  implicit def listMonad[A](l : List[A]) : ListMonadExtender[A] = ListMonadExtender(l)


  val lm = new ListMonad

    val x = 3

    val pureX = lm.myPure(x)

  def testF(a: Int): List[Char] = s"$a * 2 = ${a * 2}".toList

    val tl = List[Int](1,5,7,20101)

    val out = lm.myFlatMap(tl, testF)

  println(s"test myflatmap $out")

    val to = Some(10)
    val to2 = Some(11)

    // A function that takes an int as a param and returns an option double

    def testO(a: Int) : Option[Double] = {
      if(a > 10) Some(a + 0.5 * a)
      else None
    }

    // apply it

    val om = new OptionMonad

    val ao = om.myFlatMap(to, testO)
    val ao2 = om.myFlatMap(to2, testO)

    val opString : Option[String] = Some("Justin")

    val mapAnOption = opString.myMap{s => s.reverse}

    def main(args: Array[String]) : Unit = {
      println(s"ao = $ao")
      println(s"ao2 = $ao2")

      println(s"out = $out")

      println(s"map an option = $mapAnOption")

    }
  
}
