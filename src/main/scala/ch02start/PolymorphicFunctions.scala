package ch02start

import scala.annotation.tailrec

object PolymorphicFunctions {

  def main(args: Array[String]): Unit = {

    println(isSorted[Int](Array(1, 2, 3), _ <= _))
    println(isSorted[Int](Array(1, 2, 3, 4, 3), _ <= _))

    println(isSorted[Char](Array('a', 'b', 'c'), _ <= _))

    println(isSorted[String](Array("av", "avc", "avici"), _ <= _))

  }

  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) true
    else if (!ordered(as(0), as(1))) false
    else isSorted(as.tail, ordered)
  }
}
