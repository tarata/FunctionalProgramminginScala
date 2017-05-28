package monoids

/**
  * モノイドとは: 型Aと、法則を満たすMonoid[A]の実装のこと
  * 型とその型に対する2項演算opをあわせたもので、結合律を満たし単位元zeroを持つもののこと
  * @tparam A
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A




}

object Monoid {
    /**
    * 畳み込む総称関数
    * @tparam A
    */
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldLeft(m.zero)(m.op)
}
