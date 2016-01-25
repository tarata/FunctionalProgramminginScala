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

  def listOfN[A](n: Int, a: MyGen[A]): MyGen[List[A]] = MyGen(State.sequence(List.fill(n)(a.sample)))

  def boolean: MyGen[Boolean] = MyGen(State.map(choose(0,1).sample)(i => i > 0))

  def forAll[A](a: MyGen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): MyGen[Int] =
    MyGen(State.map(RNG.nonNegativeInt)(i => i % (stopExclusive - start)+ start))

}

case class MyGen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => MyGen[B]): MyGen[B] = MyGen(State.flatMap(sample)(a => f(a).sample))

  def listOfN(size: MyGen[Int]): MyGen[List[A]] = size.flatMap(i => MyGen(State.sequence(List.fill(i)(sample))))
}
