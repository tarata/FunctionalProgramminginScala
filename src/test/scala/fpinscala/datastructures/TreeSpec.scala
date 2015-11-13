package fpinscala.datastructures

import org.scalatest.{Matchers, FreeSpec}

class TreeSpec extends FreeSpec with Matchers {
  import Tree._

  "Exercise 3.25" - {
    "the size of Leaf is 1" in {
      Leaf(1).size shouldBe 1
    }

    "the size of Leaf with Leaf is 3" in {
      Branch(Leaf(1), Leaf(2)).size shouldBe 3
    }
  }

  "Exercise 3.26" - {
    "the maximum should be 3" in {
      maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    }
  }

}
