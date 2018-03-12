object Play1 {

    // Implement functor and monad for list

    trait Functor[F[_]] {
        def map[A,B](fa: F[A], fab : A => B): F[B]
    }

    trait Monad[F[_]] extends Functor[F] {

        def pure[A](a: A): F[A]

        def flatMap[A,B](v : F[A], f : A => F[B]) : F[B]
      
    }

    // Implement for list

    class ListMonad extends Monad[List] {

        def pure[A](a: A) = List(a)

        def flatMap[A, B](v: List[A], f: A => List[B]) = {

            v.map(f).flatten

        }

        def map[A,B](fa: List[A], fab : A => B): List[B] = {

            val what = flatMap[A,B](fa, a => pure(fab(a)))

            what
        }
    }

    val lm = new ListMonad

    val x = 3

    val pureX = lm.pure(x)

    def testF(a: Int): List[Char] = s"$a * 2 = ${a * 2}".toList

    val tl = List[Int](1,5,7,20101)

    val out = lm.flatMap(tl, testF)

    def main(args: Array[String]) : Unit = {
        println(out) 
    }
  
}