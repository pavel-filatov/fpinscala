package ch04optionsAndEithers

object EithersApp extends App {

  val e1: Either[String, Int] = Right(1)
  val e2: Either[String, Int] = Left("ERROR")
  val e3: Either[String, Int] = Right(2)
  val e4: Either[String, Int] = Left("ANOTHER ERROR")

  println("map: " + e1.map(_ + 11))
  println("map: " + e2.map(_ + 11))

  println()
  println("flatMap: " + e3.flatMap(x => Try(x / 2).toEither))
  println("flatMap: " + e3.flatMap(x => Try(x / 0).toEither))
  println("flatMap: " + e4.flatMap(x => Try(x / 1).toEither))

}
