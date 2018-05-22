package justinhj

import cats.FlatMap

import scala.concurrent.Future
import scala.language.{higherKinds, implicitConversions}
import cats._
import cats.data._
import cats.implicits._
import cats.instances.unit

object FPFromScratch {

  // Implement abstract functor and monad

  trait Functor[F[_]] {
    def myMap[A,B](fa : F[A], fab : A => B): F[B]
  }

  trait MyMonad[F[_]] extends Functor[F] {

        def myPure[A](a: A): F[A]

        def myFlatMap[A,B](v : F[A], f : A => F[B]) : F[B]

        // A Monad can implement map in terms of pure and flatmap

        def myMap[A,B](fa: F[A], fab : A => B): F[B] = {
          myFlatMap[A,B](fa, a => myPure(fab(a)))
        }

    }

    // Implement for Option

    class OptionMyMonad extends MyMonad[Option] {

      def myPure[A](a: A) = Some(a)

      def myFlatMap[A, B](fa: Option[A], f: A => Option[B]): Option[B] = {

        fa match {
          case Some(a) => f(a)
          case None => None
        }

      }

    }

    // Implicit conversion from Option to Monad extender

    case class OptionMyMonadExtender[A](private val o : Option[A]) extends OptionMyMonad {

      // need to add this function to allow single argument
      implicit def myMap[B](f: A => B) : Option[B] = myMap(o, f)
    }

    implicit def optionMonad[A](o : Option[A]) : OptionMyMonadExtender[A] = OptionMyMonadExtender(o)

  // Implement for list

    class ListMyMonad extends MyMonad[List] {

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

  case class ListMyMonadExtender[A](l : List[A]) extends ListMyMonad

  implicit def listMonad[A](l : List[A]) : ListMyMonadExtender[A] = ListMyMonadExtender(l)

  // how to extend a type without the constructor cost

  // boop a list ... which means duplicate the head (I made it up)
  implicit final class ListMonadOps[A](private val l : List[A]) extends AnyVal {
    def boop = l match {
      case hd :: tl =>
        hd :: hd :: tl
      case rest => rest
    }
  }

  val l1 = List(1,2,3)

  val booped = l1.boop


  val lm = new ListMyMonad

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

    val om = new OptionMyMonad

    val ao = om.myFlatMap(to, testO)
    val ao2 = om.myFlatMap(to2, testO)

    val opString : Option[String] = Some("Justin")

    val mapAnOption = opString.myMap{s => s.reverse}

    // List foldLeft

  def foldLeft[A, B](as : List[A])(z: B)(f : (A,B) => B) : B = {
    def foldLeftHelper(as : List[A], f : (A,B) => B, acc: B) : B = {
      as match {
        case hd :: tl =>
          foldLeftHelper(tl, f, f(hd, acc))
        case Nil =>
          acc
      }
    }

    foldLeftHelper(as, f, z)
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  def myMap2[F[_] : FlatMap,A,B,C](a: F[A], b: F[B])(f: (A, B) => C): F[C] = {

    for (
      a <- a;
      b <- b
    ) yield f(a,b)

  }

//  def myTraverse[F[_], A,B](as: List[F[A]])(f: A => F[B])(implicit m : Monad[F]) : F[List[B]] = {
//
//    as.foldRight(List.empty[B].pure[F]){
//
//      (a : F[A], acc : F[List[B]]) =>
//
//        // Map2 maps two things at once and passes them to a function that takes the two results
//        // so mapping f(a) gives Future[B]
//        // mapping Future[List[B]] gives List[B]
//        // then we can append the the f(a) to the list
//
//        m.map2(f(a), acc){(b, l) =>
//          //println(s"join $b with $l")
//          b :: l}
//    }
//
//  }




  def main(args: Array[String]) : Unit = {
      println(s"ao = $ao")
      println(s"ao2 = $ao2")

      println(s"out = $out")

    var x = myMap2(10.some, 3.some){case (a,b) =>  a + b}

    println(s"x $x")

    println(s"map an option = $mapAnOption")

    var y: List[Option[Int]] = List(1,2,3).map(x => x.some)

//    var ty  = myTraverse[Option,Int,Int](y){(a : Int) => Some(a + 3)}
//
//    println(s"my ty $ty")

  }
  
}
