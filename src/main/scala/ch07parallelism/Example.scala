package ch07parallelism

import java.util.concurrent.{ExecutorService, TimeUnit}

import Par.Par

object FirstExample {
  def sum(ints: Seq[Int]): Int = ints.foldLeft(0)(_ + _)

  def main(args: Array[String]) = {
    println(sum(Seq(1, 2, 3, 4)))
  }
}

object DivideAndConquer {
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1) {
      ints.headOption getOrElse 0
    } else {
      val middle = ints.size / 2
      val (left, right) = ints.splitAt(middle)
      sum(left) + sum(right)
    }

  def main(args: Array[String]) = {
    println(sum(Vector(1, 2, 3, 4, 5)))
  }
}

object ParAPIExample {
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      Par.unit(ints.headOption getOrElse 0)
    } else if (ints.size <= 10) {
      Par.unit(ints.sum)
    } else {
      val middle = ints.size / 2
      val (left, right) = ints.splitAt(middle)
      Par.map2(Par.fork(sum(left)), Par.fork(sum(right)))(_ + _)
    }

  def main(args: Array[String]): Unit = {
    import java.util.concurrent.Executors

    val es: ExecutorService = Executors.newFixedThreadPool(10)

    val output = Par.run(es)(sum((1 to 90).toVector)).get
    println(output)

    es.shutdown()
  }
}
