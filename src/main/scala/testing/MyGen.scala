package testing
import state.State.State
import state.{RNG, State}


object Prop {
  type SuccessCount = Int
  type FailedCase = String
}

trait Prop {
  import Prop._
  def &&(p: Prop): Prop = new Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount] = for {
      s1 <- Prop.this.check.right
      s2 <- p.check.right
    } yield s1 + s2
  }

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}


object MyGen {
  def unit[A](a: => A): MyGen[A] = MyGen(State.unit(a))

  def listOf[A](a: MyGen[A]): MyGen[List[A]] = ???

  def listOfN[A](n: Int, a: MyGen[A]): MyGen[List[A]] = MyGen(State.sequence(List.fill(n)(a.state)))

  def boolean: MyGen[Boolean] = MyGen(State.map(choose(0,1).state)(i => i > 0))

  def forAll[A](a: MyGen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): MyGen[Int] =
    MyGen(State.map(RNG.nonNegativeInt)(i => i % (stopExclusive - start)+ start))

  def union[A](g1: MyGen[A], g2: MyGen[A]): MyGen[A] =
    boolean.flatMap(b => if(b) g1 else g2)

}

case class MyGen[A](state: State[RNG, A]) {
  def flatMap[B](f: A => MyGen[B]): MyGen[B] = MyGen(State.flatMap(state)(a => f(a).state))

  def listOfN(size: MyGen[Int]): MyGen[List[A]] = size.flatMap(i => MyGen(State.sequence(List.fill(i)(state))))
}
