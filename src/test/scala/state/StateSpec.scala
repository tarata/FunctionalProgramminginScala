package state

import org.scalatest.{Matchers, FreeSpec}

class StateSpec extends FreeSpec with Matchers {
  "ints" - {
    "should return Nil when count = 0" in {
      val simpleRng = SimpleRNG(1)
      val (actual, r) = RNG.ints(0)(simpleRng)
      actual shouldBe Nil
    }

    "should return List which has 10 different int values" in {
      val simpleRng = SimpleRNG(1)
      val (actual, rng) = RNG.ints(10)(simpleRng)
      actual.size shouldBe 10
      actual.toSet.size shouldBe 10
    }
  }
  "ints2" - {
    "should return Nil when count = 0" in {
      val simpleRng = SimpleRNG(1)
      val (actual, r) = RNG.ints2(0)(simpleRng)
      actual shouldBe Nil
    }

    "should return List which has 10 different int values" in {
      val simpleRng = SimpleRNG(1)
      val (actual, r) = RNG.ints2(10)(simpleRng)
      actual.size shouldBe 10
      actual.toSet.size shouldBe 10
    }
  }
}
