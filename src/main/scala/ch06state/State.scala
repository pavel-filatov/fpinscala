package ch06state

import scala.annotation.tailrec

case class State[S, +A](run: S => (A, S)) { self =>

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    new State[S, B](s => {
      val (a, newState) = run(s)
      f(a).run(newState)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- self
      b <- sb
    } yield f(a, b)

}

object State {

  def unit[S, A](a: A): State[S, A] = State[S, A](s => (a, s))

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = {

    @tailrec
    def iter(states: List[State[S, A]], acc: State[S, List[A]]): State[S, List[A]] =
      states match {
        case s :: others => iter(others, acc flatMap (xs => s map ( x => x :: xs)))
        case Nil => acc map (_.reverse)
      }

    iter(states, unit(Nil))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}
