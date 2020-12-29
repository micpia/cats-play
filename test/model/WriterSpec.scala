package model

import org.scalatestplus.play.PlaySpec

class WriterSpec extends PlaySpec {

  "Cats Writer" should {

    "create an Writer with apply" in {
      import cats.data.Writer
      /*
      Writer[W, A] i actually alias for WriterT[Id, W, A]
       */
      val writer = Writer(Vector("It was the best of times", "it was the worst of times"), 1859)
      val writer2 = Writer.apply(Vector("It was the best of times", "it was the worst of times"), 1859)

      writer mustBe writer2
    }

    "create Writer from value with pure()" in {

      import cats.data.Writer
      import cats.implicits.catsSyntaxApplicativeId //for pure

      type Logged[A] = Writer[Vector[String], A]

      val writer = 123.pure[Logged]

      writer mustBe Writer(Vector.empty[String], 123)

    }

    "create Writer from log with tell() syntax" in {

      import cats.data.Writer
      import cats.implicits.catsSyntaxWriterId //for tell syntax

      Vector("bla", "bla2", "bla3").tell mustBe Writer(Vector("bla", "bla2", "bla3"), ())

    }

    "create writer from log and value with writer() syntax" in {

      import cats.data.Writer
      import cats.implicits.catsSyntaxWriterId //for writer syntax

      val writer = 123.writer(Vector("msg1", "msg2", "msg3"))

      writer mustBe Writer(Vector("msg1", "msg2", "msg3"), 123)

    }

    "extract value form writer with value() method" in {
      import cats.data.Writer // for writer

      Writer(Vector("msg1", "msg2", "msg3"), 123).value mustBe 123

    }

    "extract log form writer with written() method" in {
      import cats.data.Writer // for writer

      Writer(Vector("msg1", "msg2", "msg3"), 123).written mustBe Vector("msg1", "msg2", "msg3")

    }

    "extract log and value form writer with run() method" in {
      import cats.data.Writer // for writer

      val (log, value) = Writer(Vector("msg1", "msg2", "msg3"), 123).run

      log mustBe Vector("msg1", "msg2", "msg3")
      value mustBe 123

    }

    "writer in for comprehensions" in {
      import cats.data.Writer
      import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxWriterId} //for pure

      type Logged[A] = Writer[Vector[String], A]

      val writer1 = for {
        a <- 10.pure[Logged]
        _ <- Vector("a", "b", "c").tell
        b <- 32.writer(Vector("x", "y", "z"))
      } yield a + b

      writer1 mustBe Writer(Vector("a", "b", "c", "x", "y", "z"), 42)

    }

    "transform writer log with mapWritten()" in {
      import cats.data.Writer // for writer

      val writer = Writer(Vector("a", "b", "c", "x", "y", "z"), 42)

      writer.mapWritten(_.map(_.toUpperCase)).written mustBe Vector("A", "B", "C", "X", "Y", "Z")
    }

    "transform writer with bimap() and mapBoth()" in {
      import cats.data.Writer // for writer

      val writer = Writer(Vector("a", "b", "c", "x", "y", "z"), 42)

      val writer2 = writer.bimap(_.map(_.toUpperCase), _ * 100)

      writer2 mustBe Writer(Vector("A", "B", "C", "X", "Y", "Z"), 4200)

      val writer3 = writer.mapBoth { (log, value) =>

        val log2 = log.map(_.toUpperCase())
        val value2 = value * 1000
        (log2, value2)
      }

      writer3 mustBe Writer(Vector("A", "B", "C", "X", "Y", "Z"), 42000)
    }

    "clear writer with reset()" in {
      import cats.data.Writer // for writer

      val writer = Writer(Vector("a", "b", "c", "x", "y", "z"), 42)

      val writer2 = writer.reset

      writer2.written mustBe Vector.empty
    }

    "swap value and log" in {
      import cats.data.Writer // for writer

      val writer = Writer(Vector("a", "b", "c", "x", "y", "z"), 42)

      writer.swap mustBe Writer(42, Vector("a", "b", "c", "x", "y", "z"))

    }





  }

}
