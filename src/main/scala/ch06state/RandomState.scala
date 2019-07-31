package ch06state

import scala.annotation.tailrec

class RandomState {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // ex. 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, newState) = rng.nextInt
    val x =
      if (n >= 0) n
      else if (n == Int.MinValue) 0
      else -n
    (x, newState)
  }

  // ex. 6.2
  def doubleOld(rng: RNG): (Double, RNG) = {
    val (n, newState) = nonNegativeInt(rng)
    val dbl = n.toDouble / Int.MaxValue
    (dbl, newState)
  }

  // ex. 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, middleState) = rng.nextInt
    val (dbl, newState) = doubleOld(middleState)
    ((int, dbl), newState)
  }

  // ex. 6.3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (dbl, midState) = doubleOld(rng)
    val (int, newState) = midState.nextInt
    ((dbl, int), newState)
  }

  // ex. 6.3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (fst, fstState) = doubleOld(rng)
    val (sec, secState) = doubleOld(fstState)
    val (thd, newState) = doubleOld(secState)
    ((fst, sec, thd), newState)
  }

  // ex. 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(n: Int)(rng: RNG)(acc: List[Int]): (List[Int], RNG) =
      if (n == 0) (acc.reverse, rng)
      else {
        val (int, newState) = rng.nextInt
        go(n - 1)(newState)(int :: acc)
      }

    go(count)(rng)(List.empty[Int])
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, nextRng) = s(rng)
      (f(a), nextRng)
    }

  // ex. 6.5
  def double: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  // ex. 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, midState) = ra(rng)
      val (b, newState) = rb(midState)
      (f(a, b), newState)
    }

}
