package model

import cats.Eq
import utils.Printable
import utils.PrintableInstances._

final case class Cat(name: String, age: Int, color: String)

object Cat {

  Eq.instance[Cat] { (cat1, cat2) =>
    (cat1.name == cat2.name) &&
    (cat1.color == cat2.color) &&
    (cat1.age == cat2.age)
  }

  implicit val catPrintable: Printable[Cat] = (cat: Cat) => {
    val name = Printable.format(cat.name)
    val age = Printable.format(cat.age)
    val color = Printable.format(cat.color)
    s"$name is a $age year-old $color cat."
  }

}
