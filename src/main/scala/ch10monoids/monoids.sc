trait Monoid[A] {
  def bind(l: A, r: A): A

  def unit: A
}

val stringMonoid = new Monoid[String] {
  def bind(l: String, r: String): String = l ++ r

  val unit: String = ""
}

def listMonoid[A] = new Monoid[List[A]] {
  def bind(l: List[A], r: List[A]): List[A] = l ++ r

  val unit: List[A] = Nil
}

val words = List("another ", "day ", "of ", "sun ", "there ", "is ", "another ", "day")

def concatenate[A](list: List[A], m: Monoid[A]): A = list.foldLeft(m.unit)(m.bind)
def foldMap[A, B](list: List[A], m: Monoid[B])(f: A => B): B

words.foldLeft(stringMonoid.unit)(stringMonoid.bind)
concatenate(words, stringMonoid)


def parConcatenate[A](list: => List[A], m: Monoid[A]): A =
  list.length match {
    case 0 => m.unit
    case 1 => list.head
    case 2 => m.bind(list.head, list(1))
    case _ =>
      val i: Int = list.length / 2
      val (listInit, listEnd) = list.splitAt(i)
      m.bind(parConcatenate(listInit, m), parConcatenate(listEnd, m))
  }

parConcatenate(words, stringMonoid)




sealed trait WC
case class Stub(chars: String) extends WC
case class Part(leftStub: String, words: Int, rightStub: String)
