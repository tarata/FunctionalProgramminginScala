package laziness

import org.scalatest.{Matchers, FreeSpec}

class StreamSpec extends FreeSpec with Matchers {
  "Stream" - {

    "toList" - {
      "should return Nil" in {
        Stream().toList shouldBe Nil
      }

      "should return List(1,2,3)" in {
        Stream(1,2,3).toList shouldBe List(1,2,3)
      }
    }

    "drop" - {
      "should return Stream(2,3)" in {
        Stream(1,2,3).drop(1).toList shouldBe List(2,3)
      }

      "should return Empty" in {
        Stream(1,2,3).drop(4).toList shouldBe Nil
      }
    }

    "take" - {
      "should return Stream(1)" in {
        Stream(1,2,3).take(1).toList shouldBe List(1)
      }
    }

    "takeWhile" - {
      "should return Stream(1,3)" in {
        Stream(1,2,3).takeWhile(i => i%2 == 1).toList shouldBe List(1,3)
      }
    }

    "exists" - {
      "should return true" in {
        Stream(1,2,3).exists(_ == 1) shouldBe true
      }

      "should return false" in {
        Stream(1,2,3).exists(_ == 5) shouldBe false
      }
    }

    "exists2" - {
      "should return true" in {
        Stream(1,2,3).exists2(_ == 1) shouldBe true
      }

      "should return false" in {
        Stream(1,2,3).exists2(_ == 5) shouldBe false
      }
    }

    "forAll" - {
      "should return true" in {
        Stream(1,3,5).forAll(i => i%2 == 1) shouldBe true
      }

      "should return false" in {
        Stream(1,2,3).forAll(i => i%2 == 1) shouldBe false
      }
    }

    "takeWhile2" - {
      "should return Stream(1,3)" in {
        Stream(1,2,3).takeWhile2(i => i%2 == 1).toList shouldBe List(1,3)
      }
    }

    "map" - {
      "should return List(2,3,4)" in {
        Stream(1,2,3).map(_+1).toList shouldBe List(2,3,4)
      }
    }

    "append" - {
      "should return List(1,2,3,4,5)" in {
        Stream(1,2,3).append(Stream(4,5)).toList shouldBe List(1,2,3,4,5)
      }
    }

    "flatMap" - {
      "flatMap of List(1,2,3)(i => List(i,i)) should be List(1,1,2,2,3,3)" in {
        Stream(1,2,3).flatMap(i => Stream(i,i)).toList shouldBe List(1,1,2,2,3,3)
      }
    }
  }

}
