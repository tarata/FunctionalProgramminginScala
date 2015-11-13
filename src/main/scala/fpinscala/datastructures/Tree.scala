package fpinscala.datastructures

sealed trait Tree[+A] {
  def size: Int
  def depth: Int
}

case class Leaf[A](value: A) extends Tree[A] {
  override def size: Int = 1

  override def depth: Int = 1
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size: Int = left.size + right.size + 1

  override def depth: Int = (left.depth max right.depth) + 1
}

object Tree {
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }
}
