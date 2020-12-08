package model

import cats.Eq

final case class Cat(name: String, age: Int, color: String)

object Cat {

  Eq.instance[Cat] { (cat1, cat2) =>
    (cat1.name == cat2.name) &&
    (cat1.color == cat2.color) &&
    (cat1.age == cat2.age)
  }
}
