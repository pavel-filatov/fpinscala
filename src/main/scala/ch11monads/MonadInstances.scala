package ch11monads

import ch06state.State

object MonadInstances {

  val listMonad = new Monad[List] {
    def unit[A](a: A): List[A] = List(a)
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as flatMap f
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa flatMap f
  }

  def eitherMonad[L] = new Monad[({ type f[x] = Either[L, x] })#f] {
    def unit[A](a: A): Either[L, A] = Right(a)
    def flatMap[A, B](fa: Either[L, A])(f: A => Either[L, B]): Either[L, B] =
      fa.flatMap(f)
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      fa flatMap f
  }

  val listMonad2 = new Monad2[List] {
    def unit[A](a: A): List[A] = List(a)
    def compose[A, B, C](f: A => List[B])(g: B => List[C]): A => List[C] =
      a => f(a).flatMap(g)
  }

  // Ex. 11.17
  case class Id[A](value: A)

  val idMonad = new Monad[Id] {
    def unit[A](a: A): Id[A] = Id(a)
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa.value)
  }

  // Ex. 11.20
  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
      def unit[A](a: A): Reader[R, A] = Reader(r => a)
      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(st.run(r)).run(r))
    }
    def ask[R]: Reader[R, R] = Reader(r => r)
  }
}
