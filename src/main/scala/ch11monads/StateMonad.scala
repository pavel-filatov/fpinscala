package ch11monads

import ch06state.State

object StateMonad {

  // Ex. 11.2
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f
  }
}
