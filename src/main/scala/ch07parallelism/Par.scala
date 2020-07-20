package ch07parallelism

import java.util.concurrent.TimeUnit.MILLISECONDS
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par { self =>
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = as.map(asyncF(f))
    sequence(fbs)
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get()
      })

  def sequence[A](as: List[Par[A]]): Par[List[A]] = {
    val pas =
      as.foldRight(unit(List.empty[A]))((pa, acc) => map2(pa, acc)(_ :: _))
    map(pas)(_.reverse)
  }

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((x, _) => f(x))

  def map2[A, B, C](
      a: Par[A],
      b: Par[B]
  )(f: (A, B) => C, timeout: Option[Long] = None): Par[C] =
    es => {
      val af = a(es)
      val bf = b(es)
      timeout
        .map { to =>
          val c = f(af.get(to, MILLISECONDS), bf.get(to, MILLISECONDS))
          UnitFuture(c)
        }
        .getOrElse(UnitFuture(f(af.get, bf.get)))
    }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def cancel(x: Boolean): Boolean = false

    def isCancelled(): Boolean = false

    def isDone(): Boolean = true

    def get(timeout: Long, units: TimeUnit): A = get
  }

}
