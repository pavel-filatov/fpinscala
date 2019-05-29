package ch05laziness

object StreamsApp extends App {

  val a = Stream(1, 2, 3, 4, 5)
  val b = Stream(10, 20, 30, 40, 50)

  println("stream: " + a)
  println("stream: " + b)
  println("toList: " + a.toList)
  println("toList: " + b.toList)

  println()
  println("take: " + a.take(2).toList)
  println("take: " + b.take(30).toList)
  println("drop: " + a.drop(3).toList)
  println("drop: " + b.drop(30).toList)

  println()
  println("forAll: " + a.forAll(_ > 0))
  println("forAll: " + b.forAll(_ % 10 == 0))
  println("forAll: " + b.forAll(_ % 11 == 0))

  println()
  println("takeWhile: " + a.takeWhile(_ < 3).toList)
  println("takeWhile: " + a.takeWhile(_ > 3).toList)
  println("takeWhileViaFold: " + a.takeWhileViaFold(_ < 3).toList)
  println("takeWhileViaFold: " + a.takeWhileViaFold(_ > 3).toList)

}
