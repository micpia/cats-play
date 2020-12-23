package model

import cats.Eval
import org.scalatestplus.play.PlaySpec

class EvalSpec extends PlaySpec {
  "Eval" should {

    val rangeList = (1L to 1000000L).toList

    "blow when folding without Eval" in {

      def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
        as match {
          case head :: tail =>
            fn(head, foldRight(tail, acc)(fn))
          case Nil =>
            acc
        }

      intercept[StackOverflowError](foldRight(rangeList, 0L)(_ + _))

    }

    "work when folding with Eval" in {

      def evalFoldRight[A, B](as: List[A], acc: B)(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
        as match {
          case head :: tail =>
            fn(head, Eval.defer(evalFoldRight(tail, acc)(fn)))
          case Nil =>
            Eval.now(acc)
        }

      val result = evalFoldRight(rangeList, 0L)((elem, acc) => acc.map(_ + elem)).value

      result mustBe 500000500000L

    }

  }

}
