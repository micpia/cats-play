package model

import org.scalatestplus.play.PlaySpec

class StateMonadSpec extends PlaySpec {

  "State monad" should {

    "create a State" in {
      import cats.data.State

      val a = State[Int, String](state => (state, s"The state is $state"))

      val (state, result) = a.run(10).value
      val justTheState = a.runS(10).value
      val justTheResult = a.runA(10).value

      result mustBe "The state is 10"
      state mustBe 10
      justTheState mustBe state
      justTheResult mustBe result

    }

    "compose and transform state" in {
      import cats.data.State

      val step1 = State[Int, String] { num =>
        val ans = num + 1
        (ans, s"Result of step1: $ans")
      }

      val step2 = State[Int, String] { num =>
        val ans = num * 2
        (ans, s"Result of step2: $ans")
      }

      val both = for {
        a <- step1
        b <- step2
      } yield (a, b)

      val (state, result) = both.run(20).value

      state mustBe 42
      result mustBe ("Result of step1: 21", "Result of step2: 42")

    }
    "perform operations" in {

      /**
       * get extracts the state as the result;
       * set updates the state and returns unit as the result;
       * pure ignores the state and returns a supplied result;
       * inspect extracts the state via a transformation function;
       * modify updates the state using an update function.
       */

      import cats.data.State

      val getDemo = State.get[Int]
      getDemo.run(10).value mustBe (10, 10)

      val setDemo = State.set[Int](30)
      setDemo.run(10).value mustBe (30, ())

      val pureDemo = State.pure[Int, String]("Result")
      pureDemo.run(10).value mustBe (10, "Result")

      val inspectDemo = State.inspect[Int, String](x => s"$x!")
      inspectDemo.run(10).value mustBe (10, "10!")

      val modifyDemo = State.modify[Int](_ + 1)
      modifyDemo.run(10).value mustBe (11, ())
    }

    "compose state operations" in {
      import cats.data.State
      import State._

      val program: State[Int, (Int, Int, Int)] = for {
        a <- get[Int]
        _ <- set[Int](a + 1)
        b <- get[Int]
        _ <- modify[Int](_ + 1)
        c <- inspect[Int, Int](_ * 1000)
      } yield (a, b, c)

      val (state, result) = program.run(1).value

      state mustBe 3
      result mustBe (1, 2, 3000)

    }

    "Post‐Order Calculator" in {
//      import cats.data.State
//      import State._
//
//      type CalcState[A] = State[List[Int], A]
//
//      def operand(num: Int): CalcState[Int] =
//        State[List[Int], Int] { stack =>
//          (num :: stack, num)
//        }
//
//      def operator(func: (Int, Int) => Int): CalcState[Int] =
//        State[List[Int], Int] {
//          case b :: a :: tail =>
//            val ans = func(a, b)
//            (ans :: tail, ans)
//          case _ =>
//            sys.error("Fail!")
//        }
//
//      def evalOne(sym: String): CalcState[Int] =
//        sym match {
//          case "+" => operator(_ + _)
//          case "-" => operator(_ - _)
//          case "*" => operator(_ * _)
//          case "/" => operator(_ / _)
//          case num => operand(num.toInt)
//        }
//      State[List[Int], Int] { oldStack =>
//        val newStack = someTransformation(oldStack)
//        val result = someCalculation
//        (newStack, result)
//      }
    }


  }

}
