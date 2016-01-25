package state

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}


object RNG {
  type Rand[+A] = State.State[RNG, A]
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a,rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def map3[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => rng => (f(a), rng))

  //  def map4[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  //    flatMap(ra)(map3(_)(a => map3(rb)(b => f(a, b))(rng)))

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){i =>
    val mod = i % n
    if(i + (n - 1) - mod >= 0)
      rng => (mod, rng)
    else
      nonNegativeLessThan(n)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    def loop(rest: List[Rand[A]], r: RNG = rng, ret: List[A] = Nil): (List[A], RNG) = {
      rest match {
        case (h :: t) =>
          val (a, rr) = h(r)
          loop(t, rr, a :: ret)
        case Nil => (ret, r)
      }
    }
    loop(fs)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, r) if i < 0 => (- (i + 1), r)
    case (i, r) => (i, r)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, r)
  }

  def double2(rng: RNG): Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG):((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = nonNegativeInt(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG)  = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(c: Int, r1: RNG, ret: List[Int] = Nil): (List[Int], RNG) =
      if(c <= 0)
        (ret, r1)
      else {
        val (i, r2) = r1.nextInt
        loop(c - 1, r2, i :: ret)
      }
    loop(count, rng)
  }

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(rng => rng.nextInt))
}

//case class State[S, A](run: State.State)

object State {
  type State[S, +A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] = state => {
    val (a, r) = f(state)
    g(a)(r)
  }

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = flatMap(s)(a => rng => (f(a), rng))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = s => {
    def loop(rest: List[State[S, A]], r: S = s, ret: List[A] = Nil): (List[A], S) = {
      rest match {
        case (h :: t) =>
          val (a, rr) = h(r)
          loop(t, rr, a :: ret)
        case Nil => (ret, r)
      }
    }
    loop(fs)
  }
}

