package ch04optionsAndEithers

object functions {

  // ex. 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }

  def mean(xs: Seq[Double]): Option[Double] =
    Some(xs)
      .filter(_.nonEmpty)
      .map { nums =>
        val accumulator =
          nums.foldLeft((0.0, 0.0))((acc, x) => (acc._1 + x, acc._2 + 1))
        accumulator._1 / accumulator._2
      }

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    (op: Option[A]) => op.map(f)

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  // ex. 4.3
  def map2[A, B, C](op1: Option[A], op2: Option[B])(f: (A, B) => C): Option[C] =
    op1.flatMap(a => op2.map(b => f(a, b)))

  // ex. 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil): Option[List[A]]) {
      (op: Option[A], acc: Option[List[A]]) =>
        op.flatMap(x => acc.map(xs => x :: xs))
    }

  // ex. 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]]) {
      (elem: A, acc: Option[List[B]]) =>
        f(elem).flatMap(x => acc.map(xs => x :: xs))
    }

  // ex. 4.5
  def sequenceTraverse[A, B](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(x => x)

  // ex. 4.7
  def traverse[E, A, B](es: List[Either[E, A]])(f: A => Either[E, B]): Either[E, List[B]] = {
    def iter(eithers: List[Either[E, A]])(f: A => Either[E, B])(acc: Either[E, List[B]]): Either[E, List[B]] =
      eithers match {
        case Nil => acc
        case x :: xs =>
          x match {
            case Left(msg) => Left(msg)
            case Right(value) => iter(xs)(f)(acc flatMap (a => f(value) map (b => a :+ b)))
          }
      }

    iter(es)(f)(Right(Nil))
  }

  // ex. 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => Right(x))


  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[List[String], Person] =
    mkName(name).map2WithStack(mkAge(age))(Person(_, _))
}
