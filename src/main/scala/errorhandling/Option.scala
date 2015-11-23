package errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(g) => Some(f(g))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(g) => g
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = if(map(f).getOrElse(false)) this else None

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Chapter4 {
  def variance(xs: Seq[Double]): Option[Double] = {
    (try {
      Some(xs.sum / xs.size)
    } catch {
      case e: Exception => None
    }).flatMap(m =>
      try {
        Some(
          xs.map { x =>
            math.pow(x - m, 2)
          }.sum / xs.size
        )
      } catch {
        case e: Exception => None
      }
    )
  }
}