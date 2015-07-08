package gettingstarted

import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class MyModuleTest extends FunSuite with TableDrivenPropertyChecks{
  import MyModule._

  val fibCases = Table(
    ("n", "expected"),
    (0, 0),
    (1, 1),
    (2, 1),
    (3, 2),
    (4, 3),
    (5, 5),
    (6, 8)
  )

  forAll(fibCases) { (n:Int, expected:Int) =>
    test(s"Exercise 2.1 fib($n) will be $expected") {
      assert(fib(n) == expected)
    }
  }



  test("Exercise 2.2 isSorted") {
    def ordered(x:Int, y:Int):Boolean = x <= y

    assert( isSorted[Int](Array(0, 1, 2), ordered) === true )

    assert( isSorted[Int](Array(0, 2, 1), ordered) === false)
  }
}
