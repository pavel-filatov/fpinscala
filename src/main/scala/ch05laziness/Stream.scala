package ch05laziness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(a, as) => Some(a())
  }

  // ex. 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(a, as) => a() :: as().toList
  }

  // ex. 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(a, as) if n > 0 => Cons(a, () => as().take(n - 1))
    case _ => Empty
  }

  // ex. 5.2
  def drop(n: Int): Stream[A] = this match {
    case s if n == 0 => s
    case Cons(a, as) if n > 0 => as().drop(n - 1)
    case _ => Empty
  }

  // ex. 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(a, as) if p(a()) => Cons(a, () => as().takeWhile(p))
    case _ => Empty
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(a, as) => f(a(), as().foldRight(z)(f))
    case _ => z
  }

  // ex. 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(a, as) if p(a()) => as().forAll(p)
    case _ => false
  }

  // ex. 5.5
  def takeWhileViaFold(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]) { (a, acc) =>
      if (p(a)) Stream.cons(a, acc)
      else Empty
    }

  override def toString: String = this match {
    case Empty => "Empty"
    case Cons(a, as) => s"Stream(${a()}, ?)"
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](hd: () => A, tl: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = () => hd
    lazy val tail = () => tl
    Cons(head, tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
