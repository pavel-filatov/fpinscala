package ch06state


/** Implementation of a random state as a case of the general State
  *
  */
object Random {

  type Rand[+A] = State[RNG, A]

  def int: Rand[Int] = new Rand(_.nextInt)

  def nonNegativeInt: Rand[Int] = int map { i =>
    if (i >= 0) i
    else if (i == Int.MinValue) 0
    else -i
  }

  def double: Rand[Double] = nonNegativeInt map (_.toDouble / Int.MaxValue)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    for {
      a <- ra
      b <- rb
    } yield (a, b)

  def ints(n: Int): Rand[List[Int]] = State.sequence(List.fill(n)(int))

}
