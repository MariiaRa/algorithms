package ua.com.dataStructures

import org.scalatest.FunSuite
import ua.com.dataStructures.trees.{BST, RBTree}

class BSTTest extends FunSuite {

  test("can crate three") {
    val tree = BST(1, 2, 5, 4, 11, 9)

    val height = tree.height

    val newTree = tree.add(3)
    val newHeight = newTree.height

    assert(height == 5)
    assert(newHeight == 5)
  }

  test("can crate rb three") {
    val tree = RBTree(1, 2, 5)

    val height = tree.height

    assert(height == 2)

    val newTree = tree.add(6)
    assert(height == 2)
  }

  test("can remove node from tree") {
    val tree = RBTree(1, 2, 5, 4, 11, 9)

    val height = tree.height

    println(s"initial tree: $tree")

 val removed = tree.delete(2)
    println(s"removed 2: $removed")

    assert(height == 4)


  }

}
