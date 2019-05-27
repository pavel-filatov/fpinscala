package ch04optionsAndEithers

sealed trait Either[+E, +A] {
  // ex. 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(x) => Right(f(x))
  }

  // ex. 4.6
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(x) => f(x)
  }

  // ex. 4.6
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(x) => Right(x)
  }

  // ex. 4.6
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap { x => b map { y => f(x, y) } }

  // ex. 4.8
  def map2WithStack[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] =
    (this, b) match {
      case (Left(first), Left(second)) => Left(List(first, second))
      case (Left(first), _) => Left(List(first))
      case (_, Left(second)) => Left(List(second))
      case (Right(x), Right(y)) => Right(f(x, y))
    }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
