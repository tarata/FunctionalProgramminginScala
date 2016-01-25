package testing

import state.State.State
import state.{RNG, State}
import testing.Prop.{SuccessCount, FailedCase, TestCases}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
}

//trait Prop {
//  def &&(p: Prop): Prop = new Prop {
//    def check: Result = (Prop.this.check.isFalsified || p.check.isFalsified)
//  }
//
//  def check: Result
//}

case class Prop(run: (TestCases, RNG) => Result)

object MyGen {
  def unit[A](a: => A): MyGen[A] = MyGen(State.unit(a))

  def listOf[A](a: MyGen[A]): MyGen[List[A]] = ???

  def listOfN[A](n: Int, a: MyGen[A]): MyGen[List[A]] = MyGen(State.sequence(List.fill(n)(a.state)))

  def boolean: MyGen[Boolean] = MyGen(State.map(choose(0,1).state)(i => i > 0))

//  def forAll[A](a: MyGen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
//    randomStream()
//  }
//
//  def randomStream[A](g: MyGen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.state.run(rng)))

  def choose(start: Int, stopExclusive: Int): MyGen[Int] =
    MyGen(State.map(RNG.nonNegativeInt)(i => i % (stopExclusive - start)+ start))

  def union[A](g1: MyGen[A], g2: MyGen[A]): MyGen[A] =
    boolean.flatMap(b => if(b) g1 else g2)

  def weighted[A](g1: (MyGen[A], Double), g2: (MyGen[A], Double)): MyGen[A] = {
    val (a1, d1) = g1
    val (a2, d2) = g2
    val threshold = d1 / (d1 + d2)
    MyGen(State.map(choose(0,1).state)(i => i.toDouble > threshold)).flatMap(b => if(b) a2 else a1)
  }

}

case class MyGen[A](state: State[RNG, A]) {
  def flatMap[B](f: A => MyGen[B]): MyGen[B] = MyGen(State.flatMap(state)(a => f(a).state))

  def listOfN(size: MyGen[Int]): MyGen[List[A]] = size.flatMap(i => MyGen(State.sequence(List.fill(i)(state))))
}
