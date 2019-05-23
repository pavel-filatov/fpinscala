package ch04optionsAndEithers

object OptionsApp extends App {

  val op: Option[Int] = Some(1)
  val none: Option[Int] = None

  println("Testing options:")
  println("Option itself: " + Option(1))
  println("None: " + None)

  println("map over Some: " + op.map(_ + 1))
  println("map over None: " + none.map(_ + 1))

  println("flatMap over Some: " + op.flatMap(x => Some(x - 1)))
  println("flatMap over None: " + none.flatMap(x => Some(x - 1)))

  println("getOrElse on Some: " + op.getOrElse(999))
  println("getOrElse on None: " + none.getOrElse(999))

  println("orElse on Some: " + op.orElse(Some(999)))
  println("orElse on None: " + none.orElse(Some(999)))

  println("filter on Some: " + op.filter(_ < 2))
  println("filter on Some: " + op.filter(_ > 2))
  println("filter on None: " + none.filter(_ < 2))

}
