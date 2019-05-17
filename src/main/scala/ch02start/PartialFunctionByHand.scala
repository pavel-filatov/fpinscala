package ch02start

object PartialFunctionByHand {

  def main(args: Array[String]): Unit = {

    val add10 = partial1[Int, Int, Int](10, _ + _)

    println(s"add10 func: ${add10(5)}")


    println(RUR(100))
    println(USD(100))

    val curriedArith = curry[Int, Int, Int](_ + _)
    val currAdd10 = curriedArith(10)
    val currAdd5 = curriedArith(5)

    println(currAdd10(1))
    println(currAdd5(1))


  }

  def partial1[A, B, C](x: A, f: (A, B) => C): B => C = f(x, _)

  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)


  sealed abstract class Money { self =>
    val amount: Int
    override def toString: String = getClass.getSimpleName + " " + amount
  }

  case class RUR(amount: Int) extends Money
  case class USD(amount: Int) extends Money


}
