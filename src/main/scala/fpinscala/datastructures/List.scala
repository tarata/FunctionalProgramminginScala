package fpinscala.datastructures

sealed trait List[+A] {
  def head: A

  def tail: List[A]

  def setHead[T >: A](elm: T): List[T]

  //これだと出来ない
  //  def setHead[A](elm: A): List[A]


}

case object Nil extends List[Nothing] {
  override def head:Nothing = throw new NoSuchElementException("head of empty list")

  override def tail:List[Nothing] = Nil

  override def setHead[T >: Nothing](elm:T) :List[Nothing] = Nil
}

case class Cons[+A] (h:A, t: List[A]) extends List[A] {
  override def head:A = h

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

  def drop[A](l: List[A], n: Int): List[A] =
    if(n == 0)l
    else drop(l.tail, n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if(f(h)) dropWhile(t, f)
        else l
    }

  def init[A](l :List[A]): List[A] = {
    def initIter(l: List[A] = l, acc:List[A] = Nil): List[A] = {
      l match {
        case Nil => Nil
        case Cons(h, t) if t == Nil =>
          acc
        case Cons(h, t) =>
          initIter(t, List.append(acc, Cons(h, Nil)))
      }
    }
    initIter()
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h ,t) => Cons(h, append(t, a2))
    }

}
