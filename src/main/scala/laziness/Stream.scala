package laziness

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList:List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case Cons(h, t) => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 1 => t().drop(n - 1)
    case Cons(h, t) => t()
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case Cons(h, t) => t().takeWhile(p)
  }
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a)||b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if(p(a)) Stream.cons(a, b) else b)

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if(f(a)) Stream.cons(a,b) else b)

  def append[B >: A](as: Stream[B]): Stream[B] = foldRight(as)((a, b) => Stream.cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a).append(b))

  def tails: Stream[Stream[A]] = Stream.cons(this, Stream.unfold(this) {
    case Cons(h, t) => Some(t(), t())
    case Empty => None
  })
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs(a: Int, b: Int): Stream[Int] = Stream.cons(a, fibs(b, a+b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Empty
  }

  def ones: Stream[Int] = unfold(1)(one => Some((1, 1)))

  def fibs2(a: Int, b:Int): Stream[Int] = unfold((a, b)){ case (x , y) => Some((x , (y, x+y)))}

  def from2(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))
}

