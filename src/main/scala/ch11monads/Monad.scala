package ch11monads

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  // Ex. 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    def iter(fas: List[F[A]], acc: F[List[A]]): F[List[A]] = fas match {
      case Nil => map(acc)(_.reverse)
      case fa :: rest =>
        val updatedAcc: F[List[A]] =
          flatMap(acc)(as => flatMap(fa)(a => unit(a :: as)))
        iter(rest, updatedAcc)
    }

    iter(lma, unit(List.empty[A]))
  }

  def _sequence[A](lma: List[F[A]]): F[List[A]] = 
    lma.foldRight(unit(List.empty[A]))((fa, fla) => map2(fa, fla)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B])) { (a, facc) =>
      flatMap(facc) { acc => map(f(a)) { b => b :: acc } }
    }

  // Ex. 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    map(ma)(a => List.fill(n)(a))

  def _replicateM[A](n: Int, ma: F[A]): F[List[A]] = 
    sequence(List.fill(n)(ma))

  // Ex. 11.6
  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List.empty[A])) { (a, facc) =>
      flatMap(f(a)) { keep => if (keep) map(facc)(acc => a :: acc) else facc }
    }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  // Ex. 11.7
  def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // Ex. 11.12
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
}
