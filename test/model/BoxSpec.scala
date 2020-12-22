package model

import org.scalatestplus.play.PlaySpec

class BoxSpec extends PlaySpec {

  "Box" should {

    "define printable with contramap for String and Int" in {

      import utils.Printable.format
      import utils.PrintableInstances._

      format(Box("hello"))
      format(Box(1))
    }
  }

}
