package model

import cats.implicits.toFunctorOps
import org.scalatestplus.play.PlaySpec

class TreeSpec extends  PlaySpec {

  "Tree" should {

    "transform branch" in {

      val leaf1 = Tree.leaf(100)
      val leaf2 = Tree.leaf(200)

      val branch = Tree.branch(leaf1, leaf2)
      val branch2 = branch.map(_ * 2)

      branch2 mustBe Branch(Leaf(200), Leaf(400))

    }

  }

}
