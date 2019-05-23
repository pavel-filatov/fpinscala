package ch04optionsAndEithers

sealed trait Try[+A] {
  def toOption: Option[A] = this match {
    case Success(x) => Some(x)
    case Failure(_) => None
  }
  def toEither: Either[String, A] = this match {
    case Success(x) => Right(x)
    case Failure(msg) => Left(msg)
  }
}
object Try {
  def apply[A](expr: => A): Try[A] =
    try {
      val x = expr
      Success(x)
    } catch {
      case e: Exception => Failure(e.getMessage)
    }
}
case class Success[A](expr: A) extends Try[A]
case class Failure[A](error: String) extends Try[A]
