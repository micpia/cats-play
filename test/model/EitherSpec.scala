package model

import org.scalatestplus.play.PlaySpec

import scala.util.{Failure, Success, Try}

class EitherSpec extends  PlaySpec {

  "Either" should {

    import cats.MonadError
    import cats.syntax.applicative._
    import cats.syntax.monadError._ // for ensure

    val ageError = new IllegalArgumentException("Age must be greater than or equal to 18")

    def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
      age.pure[F].ensure(ageError)(_ >= 18)

    "validateAdult with passed Try monad" in {
      validateAdult[Try](18) mustBe Success(18)
      validateAdult[Try](8) mustBe Failure(ageError)

    }

    "validateAdult with passed ExceptionOr Type class" in {
      type ExceptionOr[A] = Either[Throwable, A]
      validateAdult[ExceptionOr](8) mustBe Left(ageError)
      validateAdult[ExceptionOr](-1) mustBe Left(ageError)
      validateAdult[ExceptionOr](20) mustBe Right(20)

    }
  }

}
