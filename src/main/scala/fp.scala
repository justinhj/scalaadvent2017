object Play1 {

    // Implement abstract functor and monad

    trait Functor[F[_]] {
        def map[A,B](fa: F[A], fab : A => B): F[B]
    }

    trait Monad[F[_]] extends Functor[F] {

        def pure[A](a: A): F[A]

        def flatMap[A,B](v : F[A], f : A => F[B]) : F[B]

        // A Monad can implement map in terms of pure and flatmap

        def map[A,B](fa: F[A], fab : A => B): F[B] = {
          flatMap[A,B](fa, a => pure(fab(a)))
        }
      
    }

    // Implement for Option

    class OptionMonad extends Monad[Option] {

      def pure[A](a: A) = Some(a)

      def flatMap[A, B](v: Option[A], f: A => Option[B]) = {

        v.map(f).flatten

      }

    }

    // Implement for list

    class ListMonad extends Monad[List] {

        def pure[A](a: A) = List(a)

        def flatMap[A, B](v: List[A], f: A => List[B]) = {
            v.map(f).flatten
        }


    }

    val lm = new ListMonad

    val x = 3

    val pureX = lm.pure(x)

    def testF(a: Int): List[Char] = s"$a * 2 = ${a * 2}".toList

    val tl = List[Int](1,5,7,20101)

    val out = lm.flatMap(tl, testF)

    val to = Some(10)
    val to2 = Some(11)

    // A function that takes an int as a param and returns an option double

    def testO(a: Int) : Option[Double] = {
      if(a > 10) Some(a + 0.5 * a)
      else None
    }

    // apply it

    val om = new OptionMonad

    val ao = om.flatMap(to, testO)
    val ao2 = om.flatMap(to2, testO)


    def main(args: Array[String]) : Unit = {
      println(s"ao = $ao")
      println(s"ao2 = $ao2")

      println(s"out = $out")
    }
  
}