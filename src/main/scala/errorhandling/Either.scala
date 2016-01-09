package errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E,B] = this match {
    case Right(v) => Right(f(v))
    case Left(v) => Left(v)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(v) => Left(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(_) => this
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Right(v) => b.map(bb => f(v,bb))
    case Left(v) => Left(v)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]
