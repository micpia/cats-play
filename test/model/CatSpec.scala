package model

import org.scalatestplus.play._

class CatSpec extends PlaySpec {
  "Cats" should {
    "be easily compared" in {

      val cat1 = Cat("Garfield", 38, "orange and black")
      val cat2 = Cat("Heathcliff", 33, "orange and black")

      cat1 === cat2 mustBe false
      (cat1 !== cat2) mustBe true

      val optionCat1 = Option(cat1)
      val optionCat1Copy = Option(cat1)
      val optionCat2 = Option(cat2)
      val optionCatEmpty = Option.empty[Cat]

      optionCat1 === optionCatEmpty mustBe false
      optionCat1 === optionCat2 mustBe false
      (optionCat1 !== optionCat2) mustBe true
      optionCatEmpty === optionCat2 mustBe false
      optionCat1 === optionCat1Copy mustBe true
    }
  }
}
