package ch06state

case class State[S, +A](run: S => (A, S)) {
  def get: State[S, S] = State(s => (s, s))
  def set(s: S): State[S, Unit] = State(_ => ((), s))

  def modify(f: S => S): State[S, Unit] = map(get)(s => set(f(s)))

  def unit[S, A](a: A): State[S, A] = State[S, A](s => (a, s))

  def flatMap[S, A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] =
    new State[S, B](s => {
      val (a, newState) = sa.run(s)
      f(a).run(newState)
    })

  def map[S, A, B](sa: State[S, A])(f: A => B): State[S, B] =
    flatMap(sa)(a => unit(f(a)))

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])
                      (f: (A, B) => C): State[S, C] =
    flatMap(sa) { a =>
      flatMap(sb) { b =>
        unit(f(a, b))
      }
    }
}
