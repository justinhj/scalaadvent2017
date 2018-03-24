object CatsPlay {


  import scala.language.higherKinds
  import cats.Monad
  import cats.implicits._
  import cats.Semigroupal
  import cats.Cartesian
  import cats.data.Validated
  import cats.instances.list._

  // Two ways to create the cartesian product function which relates to monoidal append except for two contexts

  def productFM[M[_]: Monad, A, B](
                                  fa: M[A],
                                  fb: M[B]
                                ): M[(A, B)] = {

    fa.flatMap{
      a =>
        fb.map(b => (a, b))
    }
  }

  def product[M[_]: Monad, A, B](
                                  fa: M[A],
                                  fb: M[B]
                                ): M[(A, B)] = {
    for (
      a <- fa;
      b <- fb
    ) yield (a,b)

  }

  def main(args: Array[String]) : Unit = {

    println("Hello")

    val o1 : Option[Int] = Some(1)
    val o2 : Option[Int] = Some(3)

    val product1: Option[(Int, Int)] = product(o1,o2)

    println(s"product1 $product1")

    val o3 : Option[Int] = None

    val product2: Option[(Int, Int)] = product(o1,o3)

    println(s"product2 $product2")

    type AllErrorsOr[A] = Either[List[String], A]

    //val checkit = Cartesian[AllErrorsOr].product(Validated.invalid[List[String], Int](List("Oh dear")), Validated.invalid[List[String], Int](List("Oops", "Oh no")))

    Cartesian[AllErrorsOr].product(
      Validated.invalid(List("Error 1")),
      Validated.invalid(List("Error 2"))
    )

    var x = 1



  }



}
