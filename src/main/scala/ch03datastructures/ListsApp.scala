package ch03datastructures

object ListsApp {

  def main(args: Array[String]): Unit = {

    val x = List(1, 2, 3, 4, 5)
    val y = List(10, 20)
    val z = List(100, 200)

    println("\nTesting foldLeft\n==========")
    println(List.foldLeftViaFoldRight(x, Nil: List[Int])((t, h) => Cons(h, t)))
    println(List.foldLeft(x, Nil: List[Int])((t, h) => Cons(h, t)))

    println("\nTesting foldRight\n==========")
    println(List.foldRight(x, Nil: List[Int])((t, h) => Cons(h, t)))
    println(List.foldRightNaive(x, Nil: List[Int])((t, h) => Cons(h, t)))

    println("\nTesting append\n==========")
    println(List.append(x, y))

    println("\nTesting concat\n==========")
    println(List.concat(List(x, y, z)))

    println("\nTesting addOne\n==========")
    println(List.addOne(x))

    println("\nTesting doublesToStrings\n==========")
    println(List.doublesToStrings(List(1.1, 2.2, 3.3)): List[String])

    println("\nTesting map\n==========")
    println(List.map(x)(_ + 1))
    println(List.map(y)(_ / 5.0))
    println(List.map(z)(x => x * x))

    println("\nTesting filter\n==========")
    println(List.filter(x)(_ % 2 != 0))

    println("\nTesting flatMap\n==========")
    println(List.flatMap(x)(i => List(i % 2 != 0)))
    println(List.flatMap(x)(i => if (i == 3 || i == 4) Nil else List(i)))

    println("\nTesting filterViaFlatMap\n==========")
    println(List.filterViaFlatMap(x)(_ < 3))
    println(List.filterViaFlatMap(x)(_ % 3 % 2 == 0))

    println("\nTesting sumElementsOf\n==========")
    println(List.sumElementsOf(y, z))
    println(List.sumElementsOf(x, y))


    println("\nTesting hasSubsequence\n==========")
    println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(4, 5, 6)))
    println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(10, 20)))
    println(List.hasSubsequence(List(1, 2, 3, 4, 5), Nil))
    println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5)))
    println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5, 6)))

  }





  def run(): Unit = {

    val x = List(1, 2, 3, 4)

    println(List.tail(x))
    println(List.drop(x, 2))
    println(List.drop(x, 3))

    println()

    println("DROP WHILE")
    println(List.dropWhile(x)(_  < 3))
    println(List.dropWhile(x)(_ < 6))

    println()

    println("INIT")
    println(List.init(x))
    println(List.init(Nil))
    println(List.init(List(1)))

    println()
    println("FOLD RIGHT")

    println(List.foldRight(x, Nil: List[Int])((tail, head) => Cons(head, tail)))

    println()
    println("LENGTH")

    println(List.length(x))
    println(List.length(Nil))


    println()
    println("FOLD LEFT")

    println(List.foldLeft(x, Nil: List[Int])((tail, head) => Cons(head, tail)))
  }

}
