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

  println()
  println("orElse: " + e1.orElse(e2))
  println("orElse: " + e2.orElse(e1))

  println()
  println("map2: " + e1.map2(e3)(_ + _))
  println("map2: " + e1.map2(e2)(_ + _))

  println()
  println("sequence: " + functions.sequence(List(e1, e3)))
  println("sequence: " + functions.sequence(List(e1, e2, e3)))
  println("sequence: " + functions.sequence(List(e2, e1, e4, e3)))
  println("sequence: " + functions.sequence(List(e4, e1, e2, e3)))

  println()
  println("traverse: " + functions.traverse(List(e1, e3))(x => Right(x * 2 + 3)))
  println("traverse: " + functions.traverse(List(e1, e2, e3, e4))(x => Right(x * 2 + 3)))
  println("traverse: " + functions.traverse(List(e1, e4, e3, e2))(x => Right(x * 2 + 3)))


  println()
  println("making a person: " + functions.mkPerson("Raul", 30))
  println("making a person: " + functions.mkPerson("", 30))
  println("making a person: " + functions.mkPerson("Noah", -30))
  println("making a person: " + functions.mkPerson("", -30))
}
