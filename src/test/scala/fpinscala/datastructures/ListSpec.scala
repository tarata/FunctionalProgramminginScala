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
      List.drop(x, 2) should be (List(3))
    }

    "drop Nil return Nil" in {
      List.drop(Nil, 1) should be (Nil)
    }
  }

  "Exercise 3.5" - {
    "dropWhile return n data removed List" in {
      val x = List(1,2,3,4,5)
      List.dropWhile(x, (y:Int) => y <= 3) should be (List(4,5))
    }
  }

  "Exercese 3.6" - {
    "init remove last element" in {
      val x = List(1,2,3,4,5)
      List.init(x) should be (List(1,2,3,4))
    }
  }
}
