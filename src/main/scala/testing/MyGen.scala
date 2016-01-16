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

  def listOfN[A](n: Int, a: MyGen[A]): MyGen[List[A]] = ???

  def forAll[A](a: MyGen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): MyGen[Int] =
    MyGen(State.map(RNG.nonNegativeInt)(i => i % (stopExclusive - start)+ start))

}

case class MyGen[A](sample: State[RNG, A])
