import ch11monads.Functor
import ch11monads.Monad
import ch11monads.MonadInstances._
import ch11monads.Monad2

val listFunctor = new Functor[List] {
  def map[A, B](as: List[A])(f: A => B): List[B] = as map f
}

val lst = (1, "a") :: (2, "b") :: (3, "c") :: Nil

listFunctor.distribute(lst)
listFunctor.map(lst) { case (a, b) => s"$b => $a" }

listFunctor.codistribute(
  Left("Error" :: "Warning" :: "Something else..." :: Nil)
)
listFunctor.codistribute(Right(1 :: 2 :: 3 :: Nil))

lst foreach println

// Monad

listMonad.map2(1 :: 2 :: Nil, "a" :: "b" :: Nil)((_, _))
listMonad.sequence(List(List(1), List(2), List(3)))

listMonad.replicateM(2, List(1, 2, 3))
listMonad.replicateM(2, List(List(1), List(2), List(3)))

listMonad.replicateM(2, List(1,2,3))


listMonad.map(1 :: 2 :: 4 :: Nil)(x => x * x)

listMonad.filterM(List(1,2,3,4,5))(x => List(x % 3 != 0))

val composed = listMonad.compose((x: Int) => List(x * 2))(y => List(y * 2))

listMonad.flatMap((1 :: 2 :: 3 :: Nil))(composed)
listMonad2.flatMap(1 :: 2 :: 3 :: Nil)(x => List(x * 2))

// Try Option Monad

optionMonad.sequence(List(Some(3), Some(2), Some(1), None, None))
optionMonad._sequence(List(Some(3), Some(2), Some(1), None, None))
optionMonad.sequence(List(Some(3), Some(2), Some(1)))
optionMonad._sequence(List(Some(3), Some(2), Some(1)))

optionMonad.replicateM(3, Some(List(1, 2, 3)))

optionMonad.filterM(List(1,2,3,4,5))(x => Some(x % 2 == 0))

// Try Either Monad


eitherMonad[String].sequence(List(Right(1), Right(2), Right(3)))
eitherMonad[String].sequence(List(Right(1), Right(2), Right(3), Left("Oops!")))

eitherMonad[String].replicateM(2, Left("Error"))
eitherMonad[String].replicateM(2, Right("It's alright"))

eitherMonad[String].filterM(List(1,2,3,4))(x => Right(x % 2 == 0))

// Try Id monad
idMonad.unit(1)
idMonad.flatMap(idMonad.unit(1))(x => Id(x * 4))
idMonad.join(Id(Id(1)))
idMonad.filterM(List(1,2,3))(x => Id(x % 2 == 1))

idMonad.sequence(List(Id(1), Id(2), Id(4)))
idMonad._sequence(List(Id(1), Id(2), Id(4)))

idMonad._replicateM(10, Id(1))
idMonad.replicateM(10, Id(1))

// Try Reader Monad

val intReader = Reader.readerMonad[Int]

intReader.unit("Hello").run(1)

intReader.map2(intReader.unit("Hello"), intReader.unit(5))((wrd, times) => wrd * times).run(0)

intReader.product(intReader.unit("Hello "), intReader.unit("World")).run(0)

intReader.codistribute(Left(intReader.unit(List("Left", "Error")))).run(0)

intReader.sequence(
  List(
    intReader.unit("Hello"),
    intReader.unit(", "),
    intReader.unit("World")
  )
).run(1)

intReader.traverse(List(1, 2, 3))(intReader.unit).run(99)

intReader.replicateM(4, intReader.unit(Some(1))).run(0)

intReader.traverse(List(1, 2, 3))(intReader.unit).run(1)