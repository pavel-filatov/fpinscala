package ch11monads

trait Monad2[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Any) => fa)(f)(())
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
}
