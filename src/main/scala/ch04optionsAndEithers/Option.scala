package ch04optionsAndEithers

sealed trait Option[+A] {

  def apply[B >: A](value: B): Option[B] = Some(value)

  def empty(): Option[A] = None

  // ex. 4.1
  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(x) => x
  }

  // ex. 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(x) => Some(f(x))
  }

  // ex. 4.1
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  // ex. 4.1
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(x => Some(x)).getOrElse(ob)

  // ex. 4.1
  def filter(f: A => Boolean): Option[A] =
    flatMap(x => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
