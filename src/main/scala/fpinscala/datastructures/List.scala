package fpinscala.datastructures

sealed trait List[+A] {
  def tail: List[A]

  def setHead[T >: A](elm: T): List[T]

  //これだと出来ない
  //  def setHead[A](elm: A): List[A]
}

case object Nil extends List[Nothing] {
  override def tail:List[Nothing] = Nil

  override def setHead[T >: Nothing](elm:T) :List[Nothing] = Nil
}

case class Cons[+A] (h:A, t: List[A]) extends List[A] {
  override def tail:List[A] = t

  override def setHead[T >: A](elm: T): List[T] = copy[T](h = elm)

//これだと出来ない
//  override def setHead[A](elm: A): List[A] = copy[A](h = elm)
}

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
