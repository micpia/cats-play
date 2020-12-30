package model

import org.scalatestplus.play.PlaySpec

class ReaderSpec extends PlaySpec {

  "Reader monad " should {

    /**
     * cats.data.Reader is a monad that allows us to sequence operations that de‐
     * pend on some input.
     */

    "create simple reader " in {
      import cats.data.Reader

      final case class Cat(name: String, favoriteFood: String)

      val catName: Reader[Cat, String] = Reader(cat => cat.name)

      catName.run(Cat("Garfield", "lasagne")) mustBe "Garfield"

    }

    "compose Readers depending on the same input" in {
      import cats.data.Reader

      final case class Cat(name: String, favoriteFood: String)

      val catName: Reader[Cat, String] = Reader(cat => cat.name)
      val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")
      val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

      val greetAndFeed: Reader[Cat, String] =
        for {
          greet <- greetKitty
          feed <- feedKitty
        } yield s"$greet. $feed."

      greetAndFeed(Cat("Garfield", "lasagne")) mustBe "Hello Garfield. Have a nice bowl of lasagne."
      greetAndFeed(Cat("Heathcliff", "junk food")) mustBe "Hello Heathcliff. Have a nice bowl of junk food."

    }

    "Exercise: Hacking on Readers" in {

      import cats.data.Reader
      import cats.implicits.catsSyntaxApplicativeId //for pure

      final case class Db(usernames: Map[Int, String], passwords: Map[String, String])
      type DbReader[A] = Reader[Db,A]

      def findUsername(userId: Int): DbReader[Option[String]] =
        Reader(db => db.usernames.get(userId))

      def checkPassword(username: String, password: String): DbReader[Boolean] =
        Reader(db => db.passwords.get(username).contains(password))

      def checkLogin(userId: Int, password: String): DbReader[Boolean] = //todo later change for transformers
        for {
          usernameOpt <- findUsername(userId)
          passOk <- usernameOpt.map { username =>
            checkPassword(username, password)
          }.getOrElse(false.pure[DbReader])
        } yield {
          passOk
        }

      val users = Map(
        1 -> "dade",
        2 -> "kate",
        3 -> "margo"
      )

      val passwords = Map(
        "dade" -> "zerocool",
        "kate" -> "acidburn",
        "margo" -> "secret"
      )

      val db = Db(users, passwords)

      checkLogin(1, "zerocool").run(db) mustBe true
      checkLogin(4, "davinci").run(db) mustBe false

    }



  }

}
