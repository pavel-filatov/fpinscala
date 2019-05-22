package ch03datastructures

object TreeApp {
  def main(args: Array[String]): Unit = {

    val tree: Tree[Int] = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(1))),
                                 Branch(Branch(Leaf(4), Leaf(7)), Leaf(5)))

    println("Tree size:\t" + Tree.size(tree) + "\nTree sizeFold:\t" + Tree.sizeFold(tree))
    println("Find maximum:\t" + Tree.maximum(tree) + "\nTree maxFold:\t" + Tree.maxFold(tree))
    println("Find depth:\t" + Tree.depth(tree) + "\nDepth via depthFold:\t" + Tree.depthFold(tree))

    println()

    println(
      "Test map:\nInitial tree:\t" + tree +
      "\nMapped tree:\t" + Tree.map(tree)(_ + 1) +
      "\nMapFolded tree:\t" + Tree.mapFold(tree)(_ + 1)
    )

  }

  def banner(msg: String): Unit = {
    val outline: String = "-" * (msg.size + 2)
    println(s"\n+$outline+\n| $msg |\n+$outline+")
  }
}
