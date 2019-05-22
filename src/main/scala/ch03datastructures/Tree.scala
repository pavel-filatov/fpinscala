package ch03datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A] {
  override def toString: String = s"$value"
}
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def toString: String = s"{$left  $right}"
}

object Tree {

  // ex. 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // ex. 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // ex. 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  // ex. 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // ex. 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(agg: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(left, right) => agg(fold(left)(f)(agg), fold(right)(f)(agg))
  }

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))

  def sizeFold[A](t: Tree[A]): Int = fold(t)(x => 1)(_ + _ + 1)

  def maxFold(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

  def depthFold[A](t: Tree[A]): Int = fold(t)(x => 1)((l, r) => (l max r) + 1)
}