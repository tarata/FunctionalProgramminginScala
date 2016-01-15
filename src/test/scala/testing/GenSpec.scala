package testing

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop._

class GenSpec extends Properties("List") {

  property("List.reverse") =
    forAll(Gen.listOf(Gen.choose(0, 100))) (ns => ns.reverse.reverse == ns) &&
    forAll(Gen.listOf(Gen.choose(0, 100))) (ns => ns.headOption == ns.reverse.lastOption)

  property("List.sum") = forAll(Gen.listOf(Gen.choose(0, 100))) {ns =>
    ns.sum == ns.reverse.sum
  } && forAll { num:Int =>
    forAll(Gen.listOf(num)) { ns =>
      // ネストさせる以外の方法がありそうな気がする
      ns.sum == num * ns.size
    }
  } && forAll(Gen.listOf(Gen.choose(0, 100))) {ns =>
    ns.sum == ns.sorted.sum
  } && forAll { num:Int =>
    forAll(Gen.listOf(num)) { ns =>
      ns.sum + num * ns.size == ns.map(_ + num).sum
    }
  }

  property("List.max") = forAll(Gen.nonEmptyListOf(Gen.choose(0, 100))) {ns =>
    ns.max == ns.reverse.max
  }
}
