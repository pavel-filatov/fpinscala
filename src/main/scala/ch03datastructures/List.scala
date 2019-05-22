package ch03datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  private def toStringIter[U](us: List[U], acc: String): String = us match {
    case Nil         => acc
    case Cons(x, xs) => toStringIter(xs, acc + ", " + x.toString)
  }

  override def toString: String = s"List(${toStringIter(tail, head.toString)})"
}

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil         => throw new Exception("tail of empty list")
    case Cons(x, xs) => xs
  }

  def drop[A](as: List[A], n: Int): List[A] = n match {
    case 0 => as
    case i if i < 0 =>
      throw new Exception("number of elements to drop must be non-negative")
    case i =>
      as match {
        case Nil         => throw new Exception("tail of empty list")
        case Cons(x, xs) => drop(xs, n - 1)
      }
  }

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _                   => as
  }

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => throw new Exception("Product of empty list")
    case ds  => foldLeft(ds, 1.0)(_ * _)
  }

  def appendNaive[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, appendNaive(t, a2))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((t, h) => Cons(h, t))

  def init[A](as: List[A]): List[A] = as match {
    case Cons(_, Nil) | Nil => Nil
    case Cons(x, xs)        => Cons(x, init(xs))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val reversed = foldLeft(as, Nil: List[A])((t, h) => Cons(h, t))
    foldLeft(reversed, z)(f)
  }

  def foldRightNaive[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(foldRightNaive(xs, z)(f), x)
  }

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val reversed =
      foldRightNaive(as, Nil: List[A])((acc, el) => append(acc, List(el)))
    foldRightNaive(reversed, z)(f)
  }

  def length[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((t, h) => Cons(h, t))


  def concat[A](lists: List[List[A]]): List[A] =
    foldLeft(lists, Nil: List[A])(append)

  // ex. 3.16
  def addOne(ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int])((t, h) => Cons(h + 1, t))

  // ex. 3.17
  def doublesToStrings(doubles: List[Double]): List[String] =
    foldRight(doubles, Nil: List[String])((t, h) => Cons(h.toString, t))

  // ex. 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((t, h) => Cons(f(h), t))

  // ex. 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((t, h) => if (f(h)) Cons(h, t) else t)

  // ex. 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((acc, elem) => append(f(elem), acc))

  // ex. 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  // ex. 3.22
  def sumElementsOf(as1: List[Int], as2: List[Int]): List[Int] = as1 match {
    case Nil => Nil
    case Cons(x, xs) =>
      as2 match {
        case Nil         => Nil
        case Cons(y, ys) => Cons(x + y, sumElementsOf(xs, ys))
      }
  }

  // ex. 3.23
  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] =
    as1 match {
      case Nil => Nil
      case Cons(x, xs) =>
        as2 match {
          case Nil         => Nil
          case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
        }
    }

  // ex. 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sub match {
      case Nil => true
      case Cons(y, ys) =>
        sup match {
          case Nil => false
          case Cons(x, xs) =>
            if (x == y) hasSubsequence(xs, ys)
            else hasSubsequence(xs, sub)
        }
    }

}
