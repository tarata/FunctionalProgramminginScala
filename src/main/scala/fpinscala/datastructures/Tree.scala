package fpinscala.datastructures

sealed trait Tree[+A] {
  def size: Int
  def depth: Int
  def map[B](f: A => B): Tree[B]
  def fold[B](x: B)(f: (B, Tree[A]) => B): B
  def size2: Int
  def depth2: Int
}

case class Leaf[A](value: A) extends Tree[A] {
  override def size: Int = 1

  override def depth: Int = 1

  override def map[B](f: A => B): Tree[B] = Leaf(f(value))

  override def fold[B](x: B)(f: (B, Tree[A]) => B): B = f(x, this)

  override def size2: Int = 1

  override def depth2: Int = 1
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size: Int = left.size + right.size + 1

  override def depth: Int = (left.depth max right.depth) + 1

  override def map[B](f: A => B): Tree[B] = Branch(left.map(f), right.map(f))

  override def fold[B](x: B)(f: (B, Tree[A]) => B): B = right.fold(left.fold(x)(f))(f)

  override def size2: Int = fold(1)((s, x) => s + x.size2)

  override def depth2: Int = fold(0)((d, t) => d + 1 max t.depth2)
}

object Tree {
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }
}
