package testing


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
  def listOf[A](a: MyGen[A]): MyGen[List[A]] = ???

  def listOfN[A](n: Int, a: MyGen[A]): MyGen[List[A]] = ???

  def forAll[A](a: MyGen[A])(f: A => Boolean): Prop = ???
}

trait MyGen[A]
