package ch04optionsAndEithers

sealed trait Option[+A] {

  def apply[B >: A](value: B): Option[B] = Some(value)

  def empty(): Option[A] = None

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(x) => x
  }

  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(x => Some(x)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
