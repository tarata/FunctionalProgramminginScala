package testing

trait MyGen[A]

trait Prop {
  def &&(p: Prop): Prop = new Prop {
    def check: Boolean = Prop.this.check && p.check
  }

  def check: Boolean
}

object MyGen {
  def listOf[A](a: MyGen[A]): MyGen[List[A]] = ???

  def listOfN[A](n: Int, a: MyGen[A]): MyGen[List[A]] = ???

  def forAll[A](a: MyGen[A])(f: A => Boolean): Prop = ???
}
