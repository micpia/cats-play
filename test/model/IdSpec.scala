package model

import org.scalatestplus.play.PlaySpec

class IdSpec extends PlaySpec {

  "Id" should {

    import cats.Monad
    import cats.syntax.flatMap._
    import cats.syntax.functor._ // for flatMap

    /**
     * generic function that performs a calculation on parameters that
     * come wrapped in a monad of the user’s choice
     */
    def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
      a.flatMap(x => b.map(y => x*x + y*y))

    "execute correctly sumSquare for List Monad" in {
      sumSquare(List(1, 2, 3), List(4, 5)) mustBe List(17, 26, 20, 29, 25, 34)
    }

    "execute correctly sumSquare for Option Monad" in {
      sumSquare(Option(3), Option(4)) mustBe Some(25)
    }

    "use cats.Id as a monad" in {
      import cats.Id

      sumSquare(3: Id[Int], 4: Id[Int]) mustBe (25: Id[Int])
      // we need : Id[Int] because Scala cannot unify types and
      //type constructors when searching for implicits

    }

    "define pure map and flatMap for Id" in {

      import cats.Id

      def pure[A](value: A): Id[A] = value
      def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)
      def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)

      pure(12) mustBe 12
      map(12)(_ * 2) mustBe 24
      flatMap(12)(_ * 2) mustBe 24
      //for Id map and flatMap are the same, nothing to unwrap

    }

  }

}
