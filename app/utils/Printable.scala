package utils

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = (input: String) => input
  implicit val intPrintable: Printable[Int] = (input: Int) => input.toString
}

object Printable {

  def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)

  def print[A](input: A)(implicit p: Printable[A]): Unit = println(format(input))
}