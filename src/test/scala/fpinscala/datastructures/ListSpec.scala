package fpinscala.datastructures

import org.scalatest.FreeSpec
import org.scalatest.Matchers._

class ListSpec extends FreeSpec {

  "Exercise 3.1" - {
    "Match Result will be x + y = 3" in {
      val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case _ => 101
      }

      x should be (3)
    }
  }

  "Exercise 3.2" - {
    "tail return rest elements" in {
      val x = List(1,2,3).tail
      x should be (List(2,3))
    }

    "when List is empty return Nil " in {
      val x = Nil.tail
      x should be (Nil)
    }
  }

  "Exercise 3.3" - {
    "setHead return replaced List" in {
      val x = List(1, 2, 3).setHead(10)
      x should be(List(10, 2, 3))
    }
  }

  "Exercise 3.4" - {
    "drop return n data removed List" in {
      val x = List(1,2,3)
      x.drop(2) should be (List(3))
    }

    "drop Nil return Nil" in {
      Nil.drop(1) should be (Nil)
    }
  }

  "Exercise 3.5" - {
    "dropWhile return n data removed List" in {
      val x = List(1,2,3,4,5)
      x.dropWhile((y:Int) => y <= 3) should be (List(4,5))
    }
  }

  "Exercese 3.6" - {
    "init remove last element" in {
      val x = List(1,2,3,4,5)
      x.init should be (List(1,2,3,4))
    }
  }

  "Exercise 3.7" - {
    /*
     * 出来ない
     * foldRightが全て展開されてから評価されていくため
     */
  }

  "Exercise 3.8" - {
    val actual = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    // println(actual)
    // Cons(1,Cons(2,Cons(3,Nil)))
    /*
     * List(1,2,3)
     * Cons(1, List(2,3)) => Cons(1, foldRight(List(2,3), Nil)(Cons(_,_))
     * Cons(1, Cons(2, List(3))
     * ただ展開してるだけ
     * array_walk的な
     */
  }

  "Exercise 3.9" - {
    "List(1,2,3)'s length will be 3" in {
      val actual = List.length(List(1,2,3))
      actual should be (3)
    }
  }

  "Exercise 3.10" - {
    // implemented
  }

  "Exercise 3.11" - {
    "sum3 of List(1,2,3) will be 6" in {
      List.sum3(List(1,2,3)) should be (6)
    }

    "product3 of List(1,2,3) will be 6" in {
      List.product3(List(1,2,3)) should be (6)
    }

    "length3 of List(1,2,3,4,5) will be 5" in {
      List.length3(List(1,2,3,4,5)) should be(5)
    }
  }

  "Exercise 3.12" - {
    "reverse of Nil will be Nil" in {
      List.reverse(Nil) should be (Nil)
    }

    "reverse of List(1,2,3) will be List(3,2,1)" in {
      List.reverse(List(1,2,3)) should be (List(3,2,1))
    }
  }

  "Exercise 3.16" - {
    "addOne of Nil should be Nil" in {
      List.addOne(Nil) shouldBe Nil
    }

    "addOne of List(1,2,3) should be List(2,3,4)" in {
      List.addOne(List(1,2,3)) shouldBe List(2,3,4)
    }
  }

  "Exercise 3.17" - {
    "doubleToString of List(1.1) should be List('1.1')" in {
      List.doubleToString(List(1.1)) shouldBe List("1.1")
    }
  }

  "Exercise 3.19" - {
    "filter of List(1,2,3,4,5) should be List(2,4)" in {
      List.filter(List(1,2,3,4,5))(i => i%2 == 0) shouldBe List(2,4)
    }
  }

  "Exercise 3.20" - {
    "flatMap of List(1,2,3)(i => List(i,i)) should be List(1,1,2,2,3,3)" in {
      List.flatMap(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
    }
  }
}
