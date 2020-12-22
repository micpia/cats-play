package utils

trait Printable[A] { self =>

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        self.format(func(value))
    }

}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = (input: String) => input
  implicit val intPrintable: Printable[Int] = (input: Int) => input.toString
}

object Printable {

  def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)

  def print[A](input: A)(implicit p: Printable[A]): Unit = println(format(input))
}

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)

    def print(implicit p: Printable[A]): Unit =
      println(format(p))
  }

}