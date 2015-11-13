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

  "Exercise 3.27" - {
    "the depth should be 3" in {
      Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).depth shouldBe 3
    }
  }

  "Exercise 3.28" - {
    "map should be applied" in {
      Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).map(_ + 1) shouldBe Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))
    }
  }

  "Exercise 3.29" - {
    "the size2 of Leaf is 1" in {
      Leaf(1).size2 shouldBe 1
    }

    "the size2 of Leaf with Leaf is 3" in {
      Branch(Leaf(1), Leaf(2)).size2 shouldBe 3
    }

    "the depth2 should be 1" in {
      Leaf(1).depth2 shouldBe 1
    }

    "the depth2 should be 2" in {
      Branch(Leaf(1), Leaf(2)).depth2 shouldBe 2
    }

    "the depth2 should be 3" in {
      Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).depth2 shouldBe 3
    }

    "the depth2 should be 4" in {
      Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(4)), Leaf(3)).depth2 shouldBe 4
    }

    "the maximum should be 3" in {
      maximum2(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    }
  }
}
