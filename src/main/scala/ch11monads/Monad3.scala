package ch11monads

trait Monad3[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def join[A](ffa: F[F[A]]): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
}
