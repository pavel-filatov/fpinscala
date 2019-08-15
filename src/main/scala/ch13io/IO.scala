package ch13io

sealed trait IO[+A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] = 
        new IO[B] { def run = f(self.run) }
    def flatMap[B](f: A => IO[B]): IO[B] = 
        new IO[B] { def run = f(self.run).run }
}
object IO {
    def unit[A](a: A): IO[A] = new IO[A] { def run: A = a }
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
    def apply[A](a: => A): IO[A] = unit(a)
}