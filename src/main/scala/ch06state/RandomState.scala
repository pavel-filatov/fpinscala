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
  def doubleNaive(rng: RNG): (Double, RNG) = {
    val (n, newState) = nonNegativeInt(rng)
    val dbl = n.toDouble / Int.MaxValue
    (dbl, newState)
  }

  // ex. 6.3
  def intDoubleNaive(rng: RNG): ((Int, Double), RNG) = {
    val (int, middleState) = rng.nextInt
    val (dbl, newState) = doubleNaive(middleState)
    ((int, dbl), newState)
  }

  // ex. 6.3
  def doubleIntNaive(rng: RNG): ((Double, Int), RNG) = {
    val (dbl, midState) = doubleNaive(rng)
    val (int, newState) = midState.nextInt
    ((dbl, int), newState)
  }

  // ex. 6.3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (fst, fstState) = doubleNaive(rng)
    val (sec, secState) = doubleNaive(fstState)
    val (thd, newState) = doubleNaive(secState)
    ((fst, sec, thd), newState)
  }

  // ex. 6.4
  def intsNaive(count: Int)(rng: RNG): (List[Int], RNG) = {
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
  def mapOld[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, nextRng) = s(rng)
      (f(a), nextRng)
    }

  // ex. 6.5
  def double: Rand[Double] = mapOld(nonNegativeInt)(_.toDouble / Int.MaxValue)

  // ex. 6.6
  def map2Old[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, midState) = ra(rng)
      val (b, newState) = rb(midState)
      (f(a, b), newState)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2Old(ra, rb)((_, _))

  def intDouble: Rand[(Int, Double)] = both(int, double)
  def doubleInt: Rand[(Double, Int)] = both(double, int)

  // ex. 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @tailrec
    def sequenceIter[A](fs: List[Rand[A]], acc: Rand[List[A]]): Rand[List[A]] =
      fs match {
        case ra :: others => sequenceIter(others, map2Old(ra, acc)(_ :: _))
        case Nil          => acc
      }
    sequenceIter(fs, unit(Nil))
  }

  // ex. 6.7
  def ints(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))

  // ex. 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, newState) = f(rng)
    g(a)(newState)
  }

  // ex. 6.8
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      if (i < n) unit(i)
      else unit(i - (i / n) * n)
    }

  // ex. 6.9
  def map[A, B](ra: Rand[A])(f: A => B): Rand[B] = flatMap(ra)(a => unit(f(a)))

  // ex. 6.9
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

}
